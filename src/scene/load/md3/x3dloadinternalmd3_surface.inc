{%MainUnit x3dloadinternalmd3.pas}
{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ MD3 surface class. }

type
  TMd3Surface = class
  private
    Shaders: TMd3ShaderList;

    { Read surface from file, and advance position to next surface. }
    procedure Read(Stream: TStream);
  public
    constructor Create;
    destructor Destroy; override;

  public
    Name: string;

    Vertexes: TMd3VertexList;
    TextureCoords: TMd3TexCoordList;
    Triangles: TMd3TriangleList;

    { Shader name, or '' if not found in file.
      Each surface may use a different "shader" which is really just a way to specify a texture
      (possibly indirectly, if using Skin dictionary).
      See sandyhead in example MD3 in https://forum.castle-engine.io/t/weirdness-with-zeronextsecondspassed-when-used-with-tupdate/775/21 }
    ShaderName: String;

    { Frames within this surface.
      This is always the same as the TObject3DMD3.NumFrames of enclosing
      TObject3DMD3 instance (yes, this assumption is checked when loading
      MD3, as it must be @true for any valid MD3 file). }
    NumFrames: Cardinal;

    { Vertexes array has VertexesInFrameCount * NumFrames items,
      each set of VertexesInFrameCount is for a different animation frame.
      TextureCoords always has VertexesInFrameCount (texture is not
      animated). }
    VertexesInFrameCount: Cardinal;
  end;

  TMd3SurfaceList = {$ifdef FPC}specialize{$endif} TObjectList<TMd3Surface>;

constructor TMd3Surface.Create;
begin
  inherited;
  Vertexes := TMd3VertexList.Create;
  Triangles := TMd3TriangleList.Create;
  TextureCoords := TMd3TexCoordList.Create;
  Shaders := TMd3ShaderList.Create;
end;

destructor TMd3Surface.Destroy;
begin
  FreeAndNil(Vertexes);
  FreeAndNil(Triangles);
  FreeAndNil(TextureCoords);
  FreeAndNil(Shaders);
  inherited;
end;

procedure TMd3Surface.Read(Stream: TStream);
type
  TMd3FileSurface = record
    Ident: array [0..3] of AnsiChar;
    Name: array [0..Md3MaxQPath - 1] of AnsiChar;
    Flags: Int32;
    NumFrames: Int32;
    NumShaders: Int32;
    NumVerts: Int32;
    NumTriangles: Int32;
    OffsetTriangles: Int32;
    OffsetShaders: Int32;
    OffsetST: Int32;
    OffsetXYZNormal: Int32;
    OffsetEnd: Int32;
  end;
var
  SurfaceStart: Int64;
  Surface: TMd3FileSurface;
begin
  SurfaceStart := Stream.Position;
  Stream.ReadBuffer(Surface, SizeOf(Surface));

  if not CompareMem(@Surface.Ident[0], @GoodIdent[1], Length(GoodIdent)) then
    raise EInvalidMD3.CreateFmt('Identifier of MD3 Surface must be "%s"',
      [GoodIdent]);

  Name := Surface.Name;
  NumFrames := Surface.NumFrames;
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

  Triangles.Count := Surface.NumTriangles;
  if Triangles.Count <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetTriangles;
    Stream.ReadBuffer(Triangles.List^, SizeOf(TMd3Triangle) * Triangles.Count);
  end;

  { For animations: we have Surface.NumFrames times the vertexes array.
    For each frame, separate collection of vertices is added. }
  Vertexes.Count := Surface.NumVerts * Surface.NumFrames;
  if Vertexes.Count <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetXYZNormal;
    Stream.ReadBuffer(Vertexes.List^, SizeOf(TMd3Vertex) * Vertexes.Count);
  end;

  Shaders.Count := Surface.NumShaders;
  if Shaders.Count <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetShaders;
    Stream.ReadBuffer(Shaders.List^, SizeOf(TMd3Shader) * Shaders.Count);

    { We only use 1st shader name.
      Interpretation of other shader names is unknown. }
    Assert(Shaders.Count <> 0);
    ShaderName := Shaders[0].Name;
  end;

  TextureCoords.Count := VertexesInFrameCount;
  if TextureCoords.Count <> 0 then
  begin
    Stream.Position := SurfaceStart + Surface.OffsetST;
    Stream.ReadBuffer(TextureCoords.List^, SizeOf(TMd3TexCoord) * TextureCoords.Count);
  end;

  Stream.Position := SurfaceStart + Surface.OffsetEnd;
end;
