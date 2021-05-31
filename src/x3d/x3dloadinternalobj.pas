{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load Wavefront OBJ 3D format.
  See [http://www.fileformat.info/format/wavefrontobj/]
  and [http://www.fileformat.info/format/material/].
  Texture URL is also read from material file. }
unit X3DLoadInternalOBJ;

{$I castleconf.inc}

interface

uses SysUtils, Classes,
  X3DNodes;

function LoadWavefrontOBJ(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

implementation

uses Generics.Collections,
  CastleStringUtils, CastleFilesUtils, CastleLog, CastleVectors, CastleUtils,
  CastleClassUtils, X3DLoadInternalUtils, CastleURIUtils,
  CastleDownload;

type
  TWavefrontMaterial = class
    Name: string;
    AmbientColor, DiffuseColor, SpecularColor, TransmissionColor: TVector3;
    IlluminationModel: Single;
    Opacity: Single;
    SpecularExponent: Single;
    Sharpness, IndexOfRefraction: Single;
    DiffuseTextureURL: string;
    BumpTextureURL: string;

    { Initializes material with default values.
      Since Wavefront specification doesn't say what the default values are,
      we just assign something along the lines of default VRML material. }
    constructor Create(const AName: string);
  end;

  TWavefrontMaterialList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TWavefrontMaterial>)
    { Find material with given name, @nil if not found. }
    function TryFindName(const Name: string): TWavefrontMaterial;
  end;

  TWavefrontFace = class
    VertIndices, TexCoordIndices, NormalIndices: TLongIntList;
    HasTexCoords: boolean;
    HasNormals: boolean;

    { Material assigned to this face. @nil means that no material was assigned. }
    Material: TWavefrontMaterial;

    constructor Create;
    destructor Destroy; override;
  end;

  TWavefrontFaceList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TWavefrontFace>;

  { 3D model in OBJ file format. }
  TObject3DOBJ = class
  strict private
    { Lists to fill with vertex, tex coord and normal data }
    Verts: TVector3List;
    TexCoords: TVector2List;
    Normals: TVector3List;

    FFaces: TWavefrontFaceList;
    FMaterials: TWavefrontMaterialList;
  public
    Coord: TCoordinateNode;
    TexCoord: TTextureCoordinateNode;
    Normal: TNormalNode;

    constructor Create(const Stream: TStream; const BaseUrl: String);
    destructor Destroy; override;

    property Faces: TWavefrontFaceList read FFaces;
    property Materials: TWavefrontMaterialList read FMaterials;
  end;

  EInvalidOBJFile = class(Exception);

{ Read string to TVector3, ignoring invalid ending,
  so it handles even incorrect things like '0 0 0c',
  see https://github.com/castle-engine/castle-engine/pull/76/ }
function Vector3FromStrPermissive(const S: string): TVector3;
const
  OnlyNums = AllChars - ['0'..'9', '.', '-','+','e','E'];
var
  SPosition: Integer;
begin
  try
    Result := Vector3FromStr(S);
  except
    on E: EConvertError do
    begin
      SPosition := 1;
      Result.Data[0] := StrToFloatDot(NextToken(S, SPosition));
      Result.Data[1] := StrToFloatDot(NextToken(S, SPosition));
      Result.Data[2] := StrToFloatDot(NextToken(S, SPosition, OnlyNums));
      if NextToken(S, SPosition) <> '' then
        raise EConvertError.Create('Expected end of data when reading vector from string');
      WritelnWarning('Invalid TVector3 format: "%s", ignored the incorrect characters at the end', [S]);
    end;
  end;
end;

{ TWavefrontMaterial --------------------------------------------------------- }

constructor TWavefrontMaterial.Create(const AName: string);
begin
  inherited Create;

  Name := AName;

  AmbientColor := Vector3(0.2, 0.2, 0.2);
  DiffuseColor := Vector3(0.8, 0.8, 0.8);
  SpecularColor := Vector3(0, 0, 0);

  { This is not necessarily good, I don't use it anywhere for now }
  TransmissionColor := Vector3(0, 0, 0);

  { Blender exported writes such illumination, I guess it's good default }
  IlluminationModel := 2;

  Opacity := 1.0;

  SpecularExponent := 1;
  Sharpness := 60;
  IndexOfRefraction := 1;

  DiffuseTextureURL := '';
  BumpTextureURL := '';
end;

{ TWavefrontMaterialList ---------------------------------------------------- }

function TWavefrontMaterialList.TryFindName(const Name: string):
  TWavefrontMaterial;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Name = Name then Exit;
  end;
  Result := nil;
end;

{ TWavefrontFace ------------------------------------------------------------- }

constructor TWavefrontFace.Create;
begin
  inherited;
  VertIndices := TLongIntList.Create;
  TexCoordIndices := TLongIntList.Create;
  NormalIndices := TLongIntList.Create;
end;

destructor TWavefrontFace.Destroy;
begin
  FreeAndNil(VertIndices);
  FreeAndNil(TexCoordIndices);
  FreeAndNil(NormalIndices);
  inherited;
end;

{ TObject3DOBJ --------------------------------------------------------------- }

constructor TObject3DOBJ.Create(const Stream: TStream; const BaseUrl: String);

  procedure ReadFacesFromOBJLine(const line: string; Material: TWavefrontMaterial);
  var
    Face: TWavefrontFace;

    { Add indexes of next face vertex based on VertexStr. }
    procedure ReadIndices(const VertexStr: string);
    var
      VertexSeekPos: Integer;

      { Reads and updates VertexStr and VertexSeekPos, and sets
        IndiceExists and adds to IndexList. IndiceExists is set to @false
        if next indice is indeed empty,
        or if we're standing at the end of VertexStr string.

        Just call it in sequence to read indices from VertexStr.
        Remember that empty indice may be followed by non-empty,
        e.g. VectorStr = '2//3' is allowed, and means that vertex
        index is 2, there's no texCoord index, and normal index is 3. }
      procedure ReadIndex(out IndiceExists: boolean;
        const IndexList: TLongIntList; const Count: Cardinal);
      var
        NewVertexSeekPos: Integer;
        Index: Integer;

      begin
        NewVertexSeekPos := VertexSeekPos;

        while (NewVertexSeekPos <= Length(VertexStr)) and
          (VertexStr[NewVertexSeekPos] <> '/') do
          Inc(NewVertexSeekPos);

        IndiceExists := NewVertexSeekPos > VertexSeekPos;
        if IndiceExists then
        begin
          { get signed Index }
          Index := StrToInt(CopyPos(VertexStr, VertexSeekPos, NewVertexSeekPos - 1));
          if Index > 0 then
            { we subtract 1, because indexed in OBJ are 1-based and we prefer 0-based }
            IndexList.Add(Index - 1) else
          if Index < 0 then
          begin
            Index := Index + Integer(Count);
            if Index < 0 then
              raise EInvalidOBJFile.Create('Invalid OBJ: Index is < 0 after summing with current count');
            IndexList.Add(Index);
          end else
            raise EInvalidOBJFile.Create('Invalid OBJ: Index is 0 (should be < 0 for relative and > 0 for absolute index)');
        end;

        { We add +1 to skip our ending '/' char.
          Note that we add this even if VertexSeekPos was already
          > Length(VertexStr), but this is harmless. }
        VertexSeekPos := NewVertexSeekPos + 1;
      end;

    var
      IndiceHasVertex, IndiceHasTexCoord, IndiceHasNormal: boolean;
    begin
      VertexSeekPos := 1;

      { read vertex index }
      ReadIndex(IndiceHasVertex, Face.VertIndices, Verts.Count);
      if not IndiceHasVertex then
        raise EInvalidOBJFile.CreateFmt(
          'Invalid OBJ vertex indexes "%s"', [VertexStr]);

      { read texCoord index }
      ReadIndex(IndiceHasTexCoord, Face.TexCoordIndices, TexCoords.Count);

      { read normal index }
      ReadIndex(IndiceHasNormal, Face.NormalIndices, Normals.Count);

      { update Face.HasXxx using IndiceHasXxx }
      Face.HasTexCoords := Face.HasTexCoords and IndiceHasTexCoord;
      Face.HasNormals := Face.HasNormals and IndiceHasNormal;
    end;

  var
    SeekPos: integer;

    function NextVertex: string;
    begin
      Result := NextToken(Line, SeekPos);
      if Result = '' then
        raise EInvalidOBJFile.CreateFmt(
          'Incomplete OBJ face specification "%s"', [Line]);
      while SCharIs(Line, SeekPos, WhiteSpaces) do Inc(SeekPos);
    end;

  begin
    { ReadIndices will eventually change this to @false, if for any
      vertex normal or texCoord will not be present. }
    Face := TWavefrontFace.Create;
    Face.HasTexCoords := true;
    Face.HasNormals := true;
    Face.Material := Material;
    Faces.Add(Face);

    SeekPos := 1;
    while SeekPos <= Length(Line) do
      ReadIndices(NextVertex);
  end;

  function ReadTexCoordFromOBJLine(const line: string): TVector2;
  var
    SeekPos: integer;
  begin
    SeekPos := 1;
    result[0] := StrToFloatDot(NextToken(line, SeekPos));
    result[1] := StrToFloatDot(NextToken(line, SeekPos));
    { nie uzywamy DeFormat - bo tex coord w OBJ moze byc 3d (z trzema
      parametrami) a my uzywamy i tak tylko dwoch pierwszych }
  end;

  { Reads single line from Wavefront OBJ or materials file.
    This takes care of stripping comments.
    If current line is empty or only comment, LineTok = ''.
    Otherwise, LineTok <> '' and LineAfterMarker contains the rest of the line. }
  procedure ReadLine(Line: string; out LineTok, LineAfterMarker: string);
  var
    SeekPosAfterMarker: Integer;
  begin
    Line := STruncateHash(Line);

    { calculate first line token }
    SeekPosAfterMarker := 1;
    LineTok := NextToken(Line, SeekPosAfterMarker);
    if LineTok <> '' then
      LineAfterMarker := Trim(SEnding(Line, SeekPosAfterMarker));
  end;

  procedure ReadMaterials(const URLSuffix: string);
  var
    IsMaterial: boolean;

    function ReadRGBColor(const Line: string): TVector3;
    var
      FirstToken: string;
    begin
      FirstToken := NextTokenOnce(Line);
      if SameText(FirstToken, 'xyz') or SameText(FirstToken, 'spectral') then
        { we can't interpret other colors than RGB, so we silently ignore them }
        Result := Vector3(1, 1, 1) else
        Result := Vector3FromStrPermissive(Line);
    end;

    procedure CheckIsMaterial(const AttributeName: string);
    begin
      if not IsMaterial then
        raise EInvalidOBJFile.CreateFmt(
          'Material not named yet, but it''s %s specified', [AttributeName]);
    end;

    function FixBumpTextureUrl(const S: string): string;
    var
      P: Integer;
    begin
      Result := S;
      P := Pos(' -bm', S);
      if P <> 0 then
        Result := Copy(S, 1, P - 1);
    end;

  var
    F: TTextReader;
    LineTok, LineAfterMarker: string;
  begin
    { Specification doesn't say what to do when multiple matlib directives
      encountered, i.e. when ReadMaterials is called multiple times.
      I simply add to Materials list. }

    { is some material in current file ? If false, then Materials is empty
      or last material is from some other file. }
    IsMaterial := false;

    try
      F := TTextReader.Create(CombineURI(BaseUrl, URLSuffix));
    except
      on E: Exception do
      begin
        WritelnWarning('Wavefront OBJ Material', E.Message);
        Exit;
      end;
    end;
    try
      while not F.Eof do
      begin
        ReadLine(F.Readln, LineTok, LineAfterMarker);
        if LineTok = '' then Continue;

        case ArrayPosText(LineTok, ['newmtl', 'Ka', 'Kd', 'Ks', 'Tf', 'illum',
          'd', 'Ns', 'sharpness', 'Ni', 'map_Kd', 'map_bump', 'bump']) of
          0: begin
               Materials.Add(TWavefrontMaterial.Create(LineAfterMarker));
               IsMaterial := true;
             end;
          1: begin
               CheckIsMaterial('ambient color (Ka)');
               Materials.Last.AmbientColor := ReadRGBColor(LineAfterMarker);
             end;
          2: begin
               CheckIsMaterial('diffuse color (Kd)');
               Materials.Last.DiffuseColor := ReadRGBColor(LineAfterMarker);
             end;
          3: begin
               CheckIsMaterial('specular color (Ks)');
               Materials.Last.SpecularColor := ReadRGBColor(LineAfterMarker);
             end;
          4: begin
               CheckIsMaterial('transmission filter color (Tf)');
               Materials.Last.TransmissionColor := ReadRGBColor(LineAfterMarker);
             end;
          5: begin
               CheckIsMaterial('illumination model (illum)');
               Materials.Last.IlluminationModel := StrToFloatDot(LineAfterMarker);
             end;
          6: begin
               CheckIsMaterial('dissolve (d)');
               Materials.Last.Opacity := StrToFloatDot(LineAfterMarker);
             end;
          7: begin
               CheckIsMaterial('specular exponent (Ns)');
               Materials.Last.SpecularExponent := StrToFloatDot(LineAfterMarker);
               if Materials.Last.SpecularExponent = 0 then
               begin
                 WritelnWarning('Wavefront material "%s" specifies specular exponent (Ns) as zero, ignoring', [
                   Materials.Last.Name
                 ]);
                 Materials.Last.SpecularExponent := 1;
               end;
             end;
          8: begin
               CheckIsMaterial('sharpness');
               Materials.Last.Sharpness := StrToFloatDot(LineAfterMarker);
             end;
          9: begin
               CheckIsMaterial('index of refraction (Ni)');
               Materials.Last.IndexOfRefraction := StrToFloatDot(LineAfterMarker);
             end;
          10:begin
               CheckIsMaterial('diffuse map (map_Kd)');
               Materials.Last.DiffuseTextureURL := LineAfterMarker;
             end;
          11, 12:
             begin
               CheckIsMaterial('bump map (map_bump,bump)');
               Materials.Last.BumpTextureURL := FixBumpTextureUrl(LineAfterMarker);
             end;
          else { we ignore other linetoks };
        end;
      end;
    finally FreeAndNil(F) end;
  end;

var
  F: TTextReader;
  LineTok, LineAfterMarker: string;
  //GroupName: string;
  UsedMaterial: TWavefrontMaterial;
begin
  inherited Create;

  FFaces := TWavefrontFaceList.Create(true);
  FMaterials := TWavefrontMaterialList.Create(true);

  UsedMaterial := nil;

  F := TTextReader.Create(Stream, false);
  try
    Coord := TCoordinateNode.Create('ObjCoordinates');
    TexCoord := TTextureCoordinateNode.Create('ObjTextureCoordinates');
    Normal := TNormalNode.Create('ObjNormals');

    Verts := Coord.FdPoint.Items;
    TexCoords := TexCoord.FdPoint.Items;
    Normals := Normal.FdVector.Items;

    while not F.Eof do
    begin
      ReadLine(F.Readln, LineTok, LineAfterMarker);

      { LineTok = '' means "this line is a comment" }
      if LineTok = '' then Continue;

      { specialized token line parsing }
      case ArrayPosText(lineTok, ['v', 'vt', 'f', 'vn', 'g', 'mtllib', 'usemtl']) of
        0: Verts.Add(Vector3FromStr(lineAfterMarker));
        1: TexCoords.Add(ReadTexCoordFromOBJLine(lineAfterMarker));
        2: ReadFacesFromOBJLine(lineAfterMarker, UsedMaterial);
        3: Normals.Add(Vector3FromStr(lineAfterMarker));
        4: {GroupName := LineAfterMarker};
        5: ReadMaterials(LineAfterMarker);
        6: begin
             if LineAfterMarker = '(null)' then
               UsedMaterial := nil else
             begin
               UsedMaterial := Materials.TryFindName(LineAfterMarker);
               if UsedMaterial = nil then
                 WritelnWarning('Wavefront OBJ', Format('Unknown material name "%s"',
                   [LineAfterMarker]));
             end;
           end;
        else { we ignore other linetoks };
      end;
    end;
  finally FreeAndNil(F) end;
end;

destructor TObject3DOBJ.Destroy;
begin
  FreeIfUnusedAndNil(Coord);
  FreeIfUnusedAndNil(TexCoord);
  FreeIfUnusedAndNil(Normal);
  FreeAndNil(FFaces);
  FreeAndNil(FMaterials);
  inherited;
end;

{ LoadWavefrontOBJ ----------------------------------------------------------- }

function LoadWavefrontOBJ(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
const
  { When constructing large index arrays, we use larger Capacity
    to make them faster.
    TODO: would be better to allocate necessary space once, by assigning Count. }
  IndicesCapacity = 100;

  function MatOBJNameToX3DName(const MatOBJName: string): string;
  begin
    Result := 'Material_' + MatOBJName;
  end;

  function MaterialToX3D(const Material: TWavefrontMaterial): TAppearanceNode;
  var
    Mat: TMaterialNode;
    Texture: TImageTextureNode;
  begin
    Result := TAppearanceNode.Create(
      MatOBJNameToX3DName(Material.Name), BaseUrl);

    Mat := TMaterialNode.Create('', BaseUrl);
    Result.Material := Mat;
    Mat.AmbientIntensity := AmbientIntensity(
      Material.AmbientColor, Material.DiffuseColor);
    Mat.DiffuseColor := Material.DiffuseColor;
    Mat.SpecularColor := Material.SpecularColor;
    Mat.Transparency := 1 - Material.Opacity;
    Mat.Shininess := Material.SpecularExponent / 128.0;

    if Material.DiffuseTextureURL <> '' then
    begin
      Texture := TImageTextureNode.Create('', BaseUrl);
      Result.Texture := Texture;
      Texture.SetUrl([SearchTextureFile(BaseUrl, Material.DiffuseTextureURL)]);

      if Material.BumpTextureURL <> '' then
      begin
        Texture := TImageTextureNode.Create('', BaseUrl);
        Result.NormalMap := Texture;
        Texture.SetUrl([SearchTextureFile(BaseUrl, Material.BumpTextureURL)]);
      end;
    end;
  end;

var
  Obj: TObject3DOBJ;
  Faces: TIndexedFaceSetNode;
  I: integer;
  FacesWithTexCoord, FacesWithNormal: boolean;
  FacesWithMaterial: TWavefrontMaterial;
  Appearances: TX3DNodeList;
  Shape: TShapeNode;
begin
  Appearances := nil;

  Result := TX3DRootNode.Create('', BaseUrl);
  try
    Result.HasForceVersion := true;
    Result.ForceVersion := X3DVersion;

    Obj := TObject3DOBJ.Create(Stream, BaseUrl);
    try
      Appearances := TX3DNodeList.Create(false);
      Appearances.Count := Obj.Materials.Count;
      for I := 0 to Obj.Materials.Count - 1 do
        Appearances[I] := MaterialToX3D(Obj.Materials[I]);

      I := 0;
      while I < Obj.Faces.Count do
      begin
        FacesWithTexCoord := Obj.Faces[I].HasTexCoords;
        FacesWithNormal := Obj.Faces[I].HasNormals;
        FacesWithMaterial := Obj.Faces[I].Material;

        Shape := TShapeNode.Create('', BaseUrl);
        Result.AddChildren(Shape);

        if FacesWithMaterial <> nil then
        begin
          { We find appearance by name, using FindName. We're sure
            that we will find it --- because we added them all to Appearances. }
          Shape.Appearance := Appearances.FindName(
            MatOBJNameToX3DName(FacesWithMaterial.Name)) as TAppearanceNode;
        end else
          Shape.Material := TMaterialNode.Create('', BaseUrl);

        { We don't do anything special for the case when FacesWithMaterial = nil
          and FacesWithTexCoord = true. This may be generated e.g. by Blender
          exporter, if Blender object has UV texture coords but no material.

          We will then just output VRML/X3D texCoord
          field, but without texture it will not have any effect.
          This is natural, and there's no reason for now to do anything else. }

        Faces := TIndexedFaceSetNode.Create('', BaseUrl);
        Shape.Geometry := Faces;
        Faces.CreaseAngle := NiceCreaseAngle;
        { faces may be concave, see https://sourceforge.net/p/castle-engine/tickets/20
          and https://sourceforge.net/p/castle-engine/tickets/19/ }
        Faces.Convex := false;
        Faces.Solid := false;
        Faces.Coord := Obj.Coord;
        Faces.FdCoordIndex.Items.Clear;
        Faces.FdCoordIndex.Items.Capacity := IndicesCapacity;
        if FacesWithTexCoord then
        begin
          Faces.TexCoord := Obj.TexCoord;
          Faces.FdTexCoordIndex.Items.Clear;
          Faces.FdTexCoordIndex.Items.Capacity := IndicesCapacity;
        end;
        if FacesWithNormal then
        begin
          Faces.Normal := Obj.Normal;
          Faces.FdNormalIndex.Items.Clear;
          Faces.FdNormalIndex.Items.Capacity := IndicesCapacity;
        end;

        { We add Faces as long as FacesWithXxx parameters stay the same.
          We know that at least the next face is Ok. }
        repeat
          Faces.FdCoordIndex.Items.AddRange(Obj.Faces[I].VertIndices);
          Faces.FdCoordIndex.Items.Add(-1);

          if FacesWithTexCoord then
          begin
            Faces.FdTexCoordIndex.Items.AddRange(Obj.Faces[I].TexCoordIndices);
            Faces.FdTexCoordIndex.Items.Add(-1);
          end;

          if FacesWithNormal then
          begin
            Faces.FdNormalIndex.Items.AddRange(Obj.Faces[I].NormalIndices);
            Faces.FdNormalIndex.Items.Add(-1);
          end;

          Inc(I);
        until (I >= Obj.Faces.Count) or
          (FacesWithTexCoord <> Obj.Faces[I].HasTexCoords) or
          (FacesWithNormal   <> Obj.Faces[I].HasNormals) or
          (FacesWithMaterial <> Obj.Faces[I].Material);
      end;

      for I := 0 to Appearances.Count - 1 do
        Appearances[I].FreeIfUnused;
    finally
      FreeAndNil(Obj);
      FreeAndNil(Appearances);
    end;
  except FreeAndNil(result); raise end;
end;

end.
