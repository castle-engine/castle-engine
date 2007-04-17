{
  Copyright 2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ TObject3dMD3 class. }
unit Object3dMD3;

interface

uses SysUtils, Classes, KambiUtils, KambiClassUtils;

{$define read_interface}

type
  TMd3Triangle = record
    Indexes: array [0..2] of LongWord;
  end;
  PMd3Triangle = ^TMd3Triangle;

  TDynArrayItem_1 = TMd3Triangle;
  PDynArrayItem_1 = PMd3Triangle;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I dynarray_1.inc}
  TDynMd3TriangleArray = TDynArray_1;

  TMd3Vertex = record
    Position: array [0..2] of SmallInt;
    Normal: SmallInt;
  end;
  PMd3Vertex = ^TMd3Vertex;

  TDynArrayItem_2 = TMd3Vertex;
  PDynArrayItem_2 = PMd3Vertex;
  {$define DYNARRAY_2_IS_STRUCT}
  {$I dynarray_2.inc}
  TDynMd3VertexArray = TDynArray_2;

  TMd3Surface = class
  private
    { Read surface from file, and advance position to next surface. }
    procedure Read(Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;

    Name: string;

    Vertexes: TDynMd3VertexArray;
    Triangles: TDynMd3TriangleArray;

    { Frames within this surface.
      This is always the same as the TObject3dMD3.FramesCount of enclosing
      TObject3dMD3 instance (yes, this assumption is checked when loading
      MD3, as it must be @true for any valid MD3 file). }
    FramesCount: Cardinal;

    { Vertexes array has VertexesInFrameCount * FramesCount items,
      each set of VertexesInFrameCount is for a different animation frame. }
    VertexesInFrameCount: Cardinal;
  end;

  TObjectsListItem_1 = TMd3Surface;
  {$I objectslist_1.inc}
  TMd3SurfacesList = TObjectsList_1;

  { MD3 (Quake3 engine model format) reader.
    TODO: textures. }
  TObject3dMD3 = class
    { Reads MD3 from a file. }
    constructor Create(const FileName: string);

    { Reads MD3 from a stream. The stream must be freely seekable
      (i.e. setting Position to any value must be supported) ---
      if unsure, you can wrap your stream in something like TMemoryStream.

      The Stream instance is not owned by this object --- which means
      that if you created it, you should also free it, this class will not
      do it for you. You can free Stream right after constructor finished
      it's work. }
    constructor Create(Stream: TStream);

    destructor Destroy; override;

    Name: string;

    Surfaces: TMd3SurfacesList;

    FramesCount: Cardinal;
  end;

  EInvalidMD3 = class(Exception);

const
  Md3XyzScale = 1/64;

{$undef read_interface}

implementation

uses VectorMath;

{ MD3 reading code is implemented based on
  [http://icculus.org/homepages/phaethon/q3a/formats/md3format.html] }

{$define read_implementation}
{$I dynarray_1.inc}
{$I dynarray_2.inc}
{$I objectslist_1.inc}

const
  GoodIdent = 'IDP3';
  GoodVersion = 15;

  { Is always 0, but for future purposes (it seems that Md3 data may
    be embedded in other things) it's a constant here. }
  Md3Start = 0;

  Md3MaxQPath = 64;

type
  TMd3Header = record
    Ident: array [0..3] of char;
    Version: LongInt;
    Name: array [0..Md3MaxQPath - 1] of char;
    Flags: LongInt;
    NumFrames: LongInt;
    NumTags: LongInt;
    NumSurfaces: LongInt;
    NumSkins: LongInt;
    OffsetFrames: LongInt;
    OffsetTags: LongInt;
    OffsetSurfaces: LongInt;
    OffsetEof: LongInt;
  end;

  TMd3Frame = record
    MinBounds: TVector3Single;
    MaxBounds: TVector3Single;
    LocalOrigin: TVector3Single;
    Radius: Single;
    Name: array [0..15] of char;
  end;

  TMd3Tag = record
    Name: array [0..Md3MaxQPath - 1] of char;
    Origin: TVector3Single;
    Axis: array [0..2] of TVector3Single;
  end;

  TMd3FileSurface = record
    Ident: array [0..3] of char;
    Name: array [0..Md3MaxQPath - 1] of char;
    Flags: LongInt;
    NumFrames: LongInt;
    NumShaders: LongInt;
    NumVerts: LongInt;
    NumTriangles: LongInt;
    OffsetTriangles: LongInt;
    OffsetShaders: LongInt;
    OffsetST: LongInt;
    OffsetXYZNormal: LongInt;
    OffsetEnd: LongInt;
  end;

{ TMd3Surface ---------------------------------------------------------------- }

constructor TMd3Surface.Create;
begin
  inherited;
  Vertexes := TDynMd3VertexArray.Create;
  Triangles := TDynMd3TriangleArray.Create;
end;

destructor TMd3Surface.Destroy;
begin
  FreeAndNil(Vertexes);
  FreeAndNil(Triangles);
  inherited;
end;

procedure TMd3Surface.Read(Stream: TStream);
var
  SurfaceStart: Int64;
  Surface: TMd3FileSurface;
  I: Integer;
begin
  SurfaceStart := Stream.Position;
  Stream.ReadBuffer(Surface, SizeOf(Surface));

  if not CompareMem(@Surface.Ident[0], @GoodIdent[1], Length(GoodIdent)) then
    raise EInvalidMD3.CreateFmt('Identifier of MD3 Surface must be "%s"',
      [GoodIdent]);

  Name := Surface.Name;
  FramesCount := Surface.NumFrames;
  VertexesInFrameCount := Surface.NumVerts;

  (* Tests:
  Writeln('Surface');
  Writeln('  Name "', Surface.Name, '"');
  Writeln('  NumFrames "', Surface.NumFrames, '"');
  Writeln('  NumShaders "', Surface.NumShaders, '"');
  Writeln('  NumVerts "', Surface.NumVerts, '"');
  Writeln('  NumTriangles "', Surface.NumTriangles, '"');
  Writeln('  OffsetTriangles "', Surface.OffsetTriangles, '"');
  Writeln('  OffsetShaders "', Surface.OffsetShaders, '"');
  Writeln('  OffsetST "', Surface.OffsetST, '"');
  Writeln('  OffsetXYZNormal "', Surface.OffsetXYZNormal, '"');
  Writeln('  OffsetEnd "', Surface.OffsetEnd, '"');
  *)

  if Surface.NumTriangles <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetTriangles;
    for I := 0 to Surface.NumTriangles - 1 do
    begin
      Triangles.IncLength;
      Stream.ReadBuffer(Triangles.Items[Triangles.High], SizeOf(TMd3Triangle));
    end;
  end;

  Vertexes.Count := Surface.NumVerts * Surface.NumFrames;
  if Vertexes.Count <> 0 then
  begin
    { For animations: actually we have here Surface.NumFrames times
      the vertexes array. For each frame, separate collection of
      vertices is added. }
    Stream.Position := SurfaceStart + Surface.OffsetXYZNormal;
    for I := 0 to Vertexes.Count - 1 do
    begin
      Stream.ReadBuffer(Vertexes.Items[I], SizeOf(TMd3Vertex));
    end;
  end;

  Stream.Position := SurfaceStart + Surface.OffsetEnd;
end;

{ TObject3dMD3 --------------------------------------------------------------- }

constructor TObject3dMD3.Create(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Create(Stream);
  finally Stream.Free end;
end;

constructor TObject3dMD3.Create(Stream: TStream);
var
  Header: TMd3Header;
  Frame: TMd3Frame;
  Tag: TMd3Tag;
  I: Integer;
  NewSurface: TMd3Surface;
begin
  inherited Create;

  Stream.ReadBuffer(Header, SizeOf(TMd3Header));

  if not CompareMem(@Header.Ident[0], @GoodIdent[1], Length(GoodIdent)) then
    raise EInvalidMD3.CreateFmt('Identifier of MD3 file must be "%s"',
      [GoodIdent]);

  if Header.Version <> GoodVersion then
    raise EInvalidMD3.CreateFmt('Only supported version of MD3 file is "%d"',
      [GoodVersion]);

  (* Tests:
  Writeln('"', Header.Ident, '"');
  Writeln('"', Header.Version, '"');
  Writeln('"', Header.Name, '"');
  Writeln('"', Header.Flags, '"');
  Writeln('NumFrames "', Header.NumFrames, '"');
  Writeln('NumTags "', Header.NumTags, '"');
  Writeln('NumSurfaces "', Header.NumSurfaces, '"');
  Writeln('NumSkins "', Header.NumSkins, '"');
  Writeln('OffsetFrames "', Header.OffsetFrames, '"');
  Writeln('OffsetTags "', Header.OffsetTags, '"');
  Writeln('OffsetSurfaces "', Header.OffsetSurfaces, '"');
  Writeln('OffsetEof "', Header.OffsetEof, '"');
  Writeln('Stream.Size ', Stream.Size); { Usually this should be = Header.OffsetEof }
  *)

  Name := Header.Name;
  FramesCount := Header.NumFrames;

  if Header.NumFrames <> 0 then
  begin
    Stream.Position := Md3Start + Header.OffsetFrames;
    for I := 0 to Header.NumFrames - 1 do
    begin
      Stream.ReadBuffer(Frame, SizeOf(Frame));
      (* Tests:
      Writeln('Frame ', I);
      Writeln('  MinBounds ', VectorToNiceStr(Frame.MinBounds));
      Writeln('  MaxBounds ', VectorToNiceStr(Frame.MaxBounds));
      Writeln('  LocalOrigin ', VectorToNiceStr(Frame.LocalOrigin));
      Writeln('  Radius ', FloatToNiceStr(Frame.Radius));
      Writeln('  Name "', Frame.Name, '"');
      *)
    end;
  end;

  if Header.NumTags <> 0 then
  begin
    Stream.Position := Md3Start + Header.OffsetTags;
    for I := 0 to Header.NumTags - 1 do
    begin
      Stream.ReadBuffer(Tag, SizeOf(Tag));
      (* Tests:
      Writeln('Tag ', I);
      Writeln('  Name "', Tag.Name, '"');
      Writeln('  Origin ', VectorToNiceStr(Tag.Origin));
      Writeln('  Axis[0] ', VectorToNiceStr(Tag.Axis[0]));
      Writeln('  Axis[1] ', VectorToNiceStr(Tag.Axis[0]));
      Writeln('  Axis[2] ', VectorToNiceStr(Tag.Axis[0]));
      *)
    end;
  end;

  Surfaces := TMd3SurfacesList.Create;

  if Header.NumSurfaces <> 0 then
  begin
    Stream.Position := Md3Start + Header.OffsetSurfaces;
    for I := 0 to Header.NumSurfaces - 1 do
    begin
      NewSurface := TMd3Surface.Create;
      NewSurface.Read(Stream);
      Surfaces.Add(NewSurface);

      if FramesCount <> NewSurface.FramesCount then
        raise EInvalidMD3.CreateFmt('Surface frame count (%d) is different than ' +
          'model''s frame count (%d)', [FramesCount, NewSurface.FramesCount]);
    end;
  end;
end;

destructor TObject3dMD3.Destroy;
begin
  FreeWithContentsAndNil(Surfaces);
  inherited;
end;

end.
