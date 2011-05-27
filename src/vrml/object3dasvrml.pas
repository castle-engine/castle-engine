{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ @abstract(Converting 3D models to VRML.
    Converts models in various formats to one or more
    (static or animation) VRML nodes.)

  @italic(Notes for implementors of other 3D model formats:)

  @unorderedList(
    @item(The proper place to "plug" another 3D model format into the engine
      is in this unit, make your format handled by LoadVRML or LoadVRMLSequence.
      This way all you
      have to do is to implement convertion, in memory, from your 3D model
      file to a VRML / X3D nodes graph (TVRMLNode instances graph).
      VRML / X3D are incredibly flexible and have many features, so it should
      be possible for any 3D format to be convertable to VRML / X3D.

      From this point, just leave to VRML / X3D handling of rendering,
      bounding volume calculation, and everything else.)

    @item(Remember to add new file format to file filters:
      possibly LoadVRML_FileFilters and LoadVRMLSequence_FileFilters.
      Remember to add extensions of your format twice
      (once for filter specific to this format, and once for line
      "All 3D models").)

    @item(Another place to fill is view3dscene MIME database.
      This is in freedesktop.org MIME database format, will allow us
      to integrate with GNOME and other desktops nicely etc.

      Simply add appopriate element to ../../view3dscene/desktop/view3dscene.xml.
      Format is self-explanatory. It's good idea to google first
      to search for standard MIME type for your model format.
      If none is found, just use application/x-???, where ??? is some short
      name for your format.

      After adding to MIME database, you want to also add it to
      ../../view3dscene/desktop/view3dscene.desktop, to indicate that
      view3dscene handles this MIME type.

      Finally, also add this to ../../view3dscene/desktop/Makefile
      in "install_thumbnailers" target, so that GNOME nautilus thumbnailers
      for this MIME types will be installed.)

    @item(You probably also want to add something to the documentation.
      At least view3dscene.php "Features" section, and possibly other places
      if it's really important format.)
  )
}
unit Object3DAsVRML;

interface

uses VectorMath, SysUtils, VRMLNodes, Object3DMD3,
  KambiUtils, Classes;

function LoadGEO(const filename: string): TVRMLRootNode;

function LoadWavefrontOBJ(const filename: string): TVRMLRootNode;

function Load3DS(const filename: string): TVRMLRootNode;

function LoadMD3(const FileName: string): TVRMLRootNode;

{ Load a specific animation frame from a given Md3 model.
  @param Md3 is the MD3 file to use.
  @param FrameNumber is the frame number to load, must be < Md3.Count.
  @param WWWBasePath is the base URL, set for TVRMLNode.WWWBasePath. }
function LoadMD3Frame(Md3: TObject3DMD3; FrameNumber: Cardinal;
  const WWWBasePath: string): TVRMLRootNode;

{ This is much like LoadVRMLSequence, but it only handles MD3 files.
  Usually you want to use LoadVRMLSequence, not this procedure. }
procedure LoadMD3Sequence(
  const FileName: string;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

{ This guesses model format basing on ExtractFileExt(filename),
  then loads model converting it to VRML.

  @param(AllowStdIn If AllowStdIn and FileName = '-' then it will load
    a VRML file from StdInStream (using GetCurrentDir as WWWBasePath).) }
function LoadVRML(const filename: string;
  AllowStdIn: boolean = false): TVRMLRootNode;

{ Deprecated name for LoadVRML. @deprecated }
function LoadAsVRML(const filename: string;
  AllowStdIn: boolean = false): TVRMLRootNode;

const
  { File filters for files loaded by LoadVRML, suitable
    for TFileFiltersList.AddFiltersFromString and
    TGLWindow.FileDialog. }
  LoadVRML_FileFilters =
  'All Files|*|' +
  '*All 3D models|*.wrl;*.wrl.gz;*.wrz;*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz;*.dae;*.iv;*.3ds;*.md3;*.obj;*.geo|' +
  'VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz|' +
  { TODO:
    and X3D binary (*.x3db;*.x3db.gz)
  }
  'X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz|' +
  'X3D classic (*.x3dv, *.x3dvz, *.x3dv.gz)|*.x3dv;*.x3dvz;*.x3dv.gz|' +
  'Collada (*.dae)|*.dae|' +
  'Inventor (*.iv)|*.iv|' +
  '3D Studio (*.3ds)|*.3ds|' +
  'Quake 3 engine models (*.md3)|*.md3|' +
  'Wavefront (*.obj)|*.obj|' +
  'Videoscape (*.geo)|*.geo';

{ Load various model formats as animation expressed by VRML sequence.

  For model formats that cannot express animations (like GEO or Wavefront OBJ)
  or that express animations in a single VRML file (like VRML >= 2.0)
  this just loads them like LoadVRML, adding exactly one item
  to RootNodes.
  This guarantees that this function handles @italic(at least)
  the same model formats as LoadVRML --- but actually it may
  handle more.

  And indeed, it currently handles kanim, that
  is completely unrecognized by LoadVRML.

  This handles animations in kanim and MD3 formats.

  @param(RootNodes Sequence of root nodes will be stored there.
    Pass here some created and empty instance of TVRMLNodesList.)

  @param(ATimes Sequence of time values.
    Pass here some created and empty instance of TDynSingleArray.)
}
procedure LoadVRMLSequence(
  const FileName: string;
  AllowStdIn: boolean;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

const
  { File filters for files loaded by LoadVRMLSequence, suitable
    for TFileFiltersList.AddFiltersFromString and
    TGLWindow.FileDialog. }
  LoadVRMLSequence_FileFilters =
  'All Files|*|' +
  '*All 3D models|*.wrl;*.wrl.gz;*.wrz;*.x3d;*.x3dz;*.x3d.gz;*.x3dv;*.x3dvz;*.x3dv.gz;*.kanim;*.dae;*.iv;*.3ds;*.md3;*.obj;*.geo|' +
  'VRML (*.wrl, *.wrl.gz, *.wrz)|*.wrl;*.wrl.gz;*.wrz|' +
  { TODO:
    X3D binary (*.x3db;*.x3db.gz)
  }
  'X3D XML (*.x3d, *.x3dz, *.x3d.gz)|*.x3d;*.x3dz;*.x3d.gz|' +
  'X3D classic (*.x3dv, *.x3dvz, *.x3dv.gz)|*.x3dv;*.x3dvz;*.x3dv.gz|' +
  'Kambi VRML engine animations (*.kanim)|*.kanim|' +
  'Collada (*.dae)|*.dae|' +
  'Inventor (*.iv)|*.iv|' +
  '3D Studio (*.3ds)|*.3ds|' +
  'Quake 3 engine models (*.md3)|*.md3|' +
  'Wavefront (*.obj)|*.obj|' +
  'Videoscape (*.geo)|*.geo';

implementation

uses Object3DGEO, Object3DS, Object3DOBJ, VRMLCameraUtils, DataErrors, VRMLErrors,
  KambiStringUtils, VRMLAnimation, ColladaToVRML, EnumerateFiles, Boxes3D;

const
  NiceCreaseAngle = DefaultVRML1CreaseAngle;

function ToVRMLName(const s: string): string;
const
  { moglibysmy tu uzyc VRMLLexer.VRMLNameChars ktore podaje naprawde
    wszystkie dozwolone znaki w nazwie VRMLa. Ale, dla czytelnosci,
    lepiej jednak nie uzywac wszystkich dziwnych znakow z
    VRMLLexer.VRMLNameChars i ograniczyc sie do ponizszego zbioru znakow }
  VRMLNameChars = ['a'..'z','A'..'Z','0'..'9'];
  NonVRMLNameChars = AllChars - VRMLNameChars;
begin
  result := SReplaceChars(s, NonVRMLNameChars, '_')
end;

{ Calculate best possible ambientIntensity. This is a float that tries to
  satisfy the equation AmbientColor = AmbientIntensity * DiffuseColor.
  Suitable for VRML 2.0/X3D Material.ambientIntensity (as there's no
  Material.ambientColor in VRML 2.0/X3D). }
function AmbientIntensity(const AmbientColor, DiffuseColor: TVector3Single): Single;
begin
  Result := 0;
  if not Zero(DiffuseColor[0]) then Result += AmbientColor[0] / DiffuseColor[0];
  if not Zero(DiffuseColor[1]) then Result += AmbientColor[1] / DiffuseColor[1];
  if not Zero(DiffuseColor[2]) then Result += AmbientColor[2] / DiffuseColor[2];
  Result /= 3;
end;

function AmbientIntensity(const AmbientColor, DiffuseColor: TVector4Single): Single;
begin
  Result := AmbientIntensity(
    Vector3SingleCut(AmbientColor),
    Vector3SingleCut(DiffuseColor));
end;

{ Search harder for filename Base inside directory Path.
  Path must be absolute and contain the final PathDelim.
  Returns filename relative to Path.

  We prefer to return just Base, if it exists, or when no alternative exists.
  When Base doesn't exist but some likely alternative exists (e.g. with
  different case), we return it. }
function SearchTextureFileName(const Path, Base: string): string;
var
  SomePathDelim: Integer;
  BaseShort: string;
begin
  try
    if SearchFileHard(Path, Base, Result) then
      Exit;

    { According to https://sourceforge.net/tracker/index.php?func=detail&aid=3305661&group_id=200653&atid=974391
      some archives expect search within textures/ subdirectory.
      Example on http://www.gfx-3d-model.com/2008/06/house-07/#more-445
      for Wavefront OBJ. }
    if SearchFileHard(Path + 'textures' + PathDelim, Base, Result) then
    begin
      Result := 'textures/' + Result;
      Exit;
    end;

    { Some invalid models place full (absolute) path inside texture filename.
      Try to handle it, by stripping path part (from any OS), and trying
      to match new name. }
    SomePathDelim := BackCharsPos(['/', '\'], Base);
    if SomePathDelim <> 0  then
    begin
      BaseShort := SEnding(Base, SomePathDelim + 1);

      if SearchFileHard(Path, BaseShort, Result) then
        Exit;
      if SearchFileHard(Path + 'textures' + PathDelim, BaseShort, Result) then
      begin
        Result := 'textures/' + Result;
        Exit;
      end;
    end;

  finally
    if Result <> Base then
      { Texture file found, but not under original name }
      DataWarning(Format('Exact texture filename "%s" not found, using instead "%s"',
        [Base, Result]));
  end;

  { default result if nowhere found }
  Result := Base;
end;

{ Load* ---------------------------------------------------------------------- }

function LoadGEO(const filename: string): TVRMLRootNode;
var
  geo: TObject3DGEO;
  verts: TNodeCoordinate;
  faces: TNodeIndexedFaceSet_2;
  Shape: TNodeShape;
  i: integer;
  WWWBasePath: string;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(filename));
  geo := TObject3DGEO.Create(filename);
  try
    result := TVRMLRootNode.Create('', WWWBasePath);
    try
      Result.HasForceVersion := true;
      Result.ForceVersion := VRML2Version;

      Shape := TNodeShape.Create('', WWWBasePath);
      result.FdChildren.Add(Shape);
      Shape.Material := TNodeMaterial_2.Create('', WWWBasePath);

      faces := TNodeIndexedFaceSet_2.Create('', WWWBasePath);
      Shape.FdGeometry.Value := faces;
      faces.FdCreaseAngle.Value := NiceCreaseAngle;
      faces.FdSolid.Value := false;
      faces.FdCoordIndex.Count := geo.Faces.Count * 4;
      for i := 0 to geo.Faces.Count-1 do
      begin
        faces.FdCoordIndex.Items[i * 4    ] := geo.Faces.Items[i][0];
        faces.FdCoordIndex.Items[i * 4 + 1] := geo.Faces.Items[i][1];
        faces.FdCoordIndex.Items[i * 4 + 2] := geo.Faces.Items[i][2];
        faces.FdCoordIndex.Items[i * 4 + 3] := -1;
      end;

      verts := TNodeCoordinate.Create('', WWWBasePath);
      faces.FdCoord.Value := verts;
      verts.FdPoint.Items.Assign(geo.Verts);
    except result.Free; raise end;
  finally geo.Free end;
end;

function LoadWavefrontOBJ(const filename: string): TVRMLRootNode;
const
  { When constructing large index arrays, we use larger AllowedCapacityOverflow
    to make them faster.
    TODO: would be better to allocate necessary space once, by assigning Count. }
  AllowedIndicesArraysOverflows = 100;
var
  WWWBasePath: string;

  function MatOBJNameToVRMLName(const MatOBJName: string): string;
  begin
    Result := 'Material_' + ToVRMLName(MatOBJName);
  end;

  function MaterialToVRML(const Material: TWavefrontMaterial): TNodeAppearance;
  var
    Mat: TNodeMaterial_2;
    Texture: TNodeImageTexture;
  begin
    Result := TNodeAppearance.Create(
      MatOBJNameToVRMLName(Material.Name), WWWBasePath);

    Mat := TNodeMaterial_2.Create('', WWWBasePath);
    Result.FdMaterial.Value := Mat;
    Mat.FdAmbientIntensity.Value := AmbientIntensity(
      Material.AmbientColor, Material.DiffuseColor);
    Mat.FdDiffuseColor.Value := Material.DiffuseColor;
    Mat.FdSpecularColor.Value := Material.SpecularColor;
    Mat.FdTransparency.Value := 1 - Material.Opacity;
    Mat.FdShininess.Value := Material.SpecularExponent / 128.0;

    if Material.DiffuseTextureFileName <> '' then
    begin
      Texture := TNodeImageTexture.Create('', WWWBasePath);
      Result.FdTexture.Value := Texture;
      Texture.FdUrl.Items.Add(SearchTextureFileName(WWWBasePath, Material.DiffuseTextureFileName));

      if Material.BumpTextureFileName <> '' then
      begin
        Texture := TNodeImageTexture.Create('', WWWBasePath);
        Result.FdNormalMap.Value := Texture;
        Texture.FdUrl.Items.Add(SearchTextureFileName(WWWBasePath, Material.BumpTextureFileName));
      end;
    end;
  end;

var
  Obj: TObject3DOBJ;
  Coord: TNodeCoordinate;
  Faces: TNodeIndexedFaceSet_2;
  TexCoord: TNodeTextureCoordinate;
  i: integer;
  FacesWithTexCoord, FacesWithNormal: boolean;
  Normal: TNodeNormal;
  FacesWithMaterial: TWavefrontMaterial;
  Appearances: TVRMLNodesList;
  Shape: TNodeShape;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(filename));
  Appearances := nil;
  Obj := TObject3DOBJ.Create(filename);
  try
    result := TVRMLRootNode.Create('', WWWBasePath);
    try
      Result.HasForceVersion := true;
      Result.ForceVersion := VRML2Version;

      Appearances := TVRMLNodesList.Create;
      Appearances.Count := Obj.Materials.Count;
      for I := 0 to Obj.Materials.Count - 1 do
        Appearances[I] := MaterialToVRML(Obj.Materials[I]);

      Coord := TNodeCoordinate.Create('',WWWBasePath);
      Coord.FdPoint.Items.Assign(obj.Verts);

      TexCoord := TNodeTextureCoordinate.Create('', WWWBasePath);
      TexCoord.FdPoint.Items.Assign(obj.TexCoords);

      Normal := TNodeNormal.Create('', WWWBasePath);
      Normal.FdVector.Items.Assign(Obj.Normals);

      i := 0;
      while i < obj.Faces.Count do
      begin
        FacesWithTexCoord := Obj.Faces.Items[i].HasTexCoords;
        FacesWithNormal := Obj.Faces.Items[i].HasNormals;
        FacesWithMaterial := Obj.Faces.Items[i].Material;

        Shape := TNodeShape.Create('', WWWBasePath);
        Result.FdChildren.Add(Shape);

        if FacesWithMaterial <> nil then
        begin
          { We find appearance by name, using FindNodeByName. We're sure
            that we will find it --- because we added them all to Appearances. }
          Shape.Appearance := Appearances[Appearances.FindNodeName(
            MatOBJNameToVRMLName(FacesWithMaterial.Name))] as TNodeAppearance;
        end else
          Shape.Material := TNodeMaterial_2.Create('', WWWBasePath);

        { We don't do anything special for the case when FacesWithMaterial = nil
          and FacesWithTexCoord = true. This may be generated e.g. by Blender
          exporter, if Blender object has UV texture coords but no material.

          We will then just output VRML/X3D texCoord
          field, but without texture it will not have any effect.
          This is natural, and there's no reason for now to do anything else. }

        Faces := TNodeIndexedFaceSet_2.Create('', WWWBasePath);
        Shape.FdGeometry.Value := Faces;
        Faces.FdCreaseAngle.Value := NiceCreaseAngle;
        Faces.FdSolid.Value := false;
        Faces.FdCoord.Value := Coord;
        Faces.FdCoordIndex.Items.SetLength(0);
        Faces.FdCoordIndex.Items.AllowedCapacityOverflow := AllowedIndicesArraysOverflows;
        if FacesWithTexCoord then
        begin
          Faces.FdTexCoord.Value := TexCoord;
          Faces.FdTexCoordIndex.Items.SetLength(0);
          Faces.FdTexCoordIndex.Items.AllowedCapacityOverflow := AllowedIndicesArraysOverflows;
        end;
        if FacesWithNormal then
        begin
          Faces.FdNormal.Value := Normal;
          Faces.FdNormalIndex.Items.SetLength(0);
          Faces.FdNormalIndex.Items.AllowedCapacityOverflow := AllowedIndicesArraysOverflows;
        end;

        { We add Faces as long as FacesWithXxx parameters stay the same.
          We know that at least the next face is Ok. }
        repeat
          Faces.FdCoordIndex.Items.AppendArray(
            [obj.Faces.Items[i].VertIndices[0],
             obj.Faces.Items[i].VertIndices[1],
             obj.Faces.Items[i].VertIndices[2], -1]);

          if FacesWithTexCoord then
            Faces.FdTexCoordIndex.Items.AppendArray(
              [obj.Faces.Items[i].TexCoordIndices[0],
               obj.Faces.Items[i].TexCoordIndices[1],
               obj.Faces.Items[i].TexCoordIndices[2], -1]);

          if FacesWithNormal then
            Faces.FdNormalIndex.Items.AppendArray(
              [obj.Faces.Items[i].NormalIndices[0],
               obj.Faces.Items[i].NormalIndices[1],
               obj.Faces.Items[i].NormalIndices[2], -1]);

          Inc(i);
        until (i >= obj.Faces.Count) or
          (FacesWithTexCoord <> obj.Faces.Items[i].HasTexCoords) or
          (FacesWithNormal   <> obj.Faces.Items[i].HasNormals) or
          (FacesWithMaterial <> obj.Faces.Items[i].Material);

        Faces.FdCoordIndex.Items.AllowedCapacityOverflow := 4;
        Faces.FdTexCoordIndex.Items.AllowedCapacityOverflow := 4;
        Faces.FdNormalIndex.Items.AllowedCapacityOverflow := 4;
      end;

      if Coord <> nil then
      begin
        Coord.FreeIfUnused;
        Coord := nil;
      end;

      if TexCoord <> nil then
      begin
        TexCoord.FreeIfUnused;
        TexCoord := nil;
      end;

      if Normal <> nil then
      begin
        Normal.FreeIfUnused;
        Normal := nil;
      end;

      for I := 0 to Appearances.Count - 1 do
        Appearances[I].FreeIfUnused;
    except FreeAndNil(result); raise end;
  finally
    FreeAndNil(obj);
    FreeAndNil(Appearances);
  end;
end;

function Load3DS(const filename: string): TVRMLRootNode;
var
  WWWBasePath: string;
  O3ds: TScene3DS;

  { Prefix names with things like "Material_", to make sure these
    names not collide with each other. (I don't know if they are in
    separate namespace in 3DS, but better be safe.)

    Note that we don't check here whether all names are really unique,
    as they don't have to be unique for VRML/X3D. We just make reasonable
    effort to keep them unique (to help storing them with DEF/USE),
    if they were unique in 3DS. }

  function MaterialVRMLName(const Mat3dsName: string): string;
  begin Result := 'Material_' + ToVRMLName(Mat3dsName) end;

  function TrimeshVRMLName(const Tri3dsName: string): string;
  begin Result := 'Trimesh_' + ToVRMLName(Tri3dsName) end;

  function ViewpointVRMLName(const Camera3dsName: string): string;
  begin Result := 'Camera_' + ToVRMLName(Camera3dsName) end;

  function LightVRMLName(const Light3dsName: string): string;
  begin Result := 'Light_' + ToVRMLName(Light3dsName) end;

  procedure AddViewpoints;
  var
    Viewpoint: TVRMLNode;
    I: Integer;
  begin
    for I := 0 to O3ds.Cameras.Count - 1 do
    begin
      Viewpoint := MakeVRMLCameraNode(2, WWWBasePath,
        O3ds.Cameras[I].Position,
        O3ds.Cameras[I].Direction,
        O3ds.Cameras[I].Up,
        O3ds.Cameras[I].Up { GravityUp equals Up });
      Viewpoint.NodeName := ViewpointVRMLName(O3ds.Cameras[I].Name);
      Result.FdChildren.Add(Viewpoint);

      { TODO: use other 3ds camera fields }
    end;
  end;

  procedure AddLights;
  var
    I: Integer;
    Light: TNodePointLight_2;
  begin
    for I := 0 to O3ds.Lights.Count - 1 do
    begin
      Light := TNodePointLight_2.Create(LightVRMLName(
        O3ds.Lights[I].Name), WWWBasePath);
      Result.FdChildren.Add(Light);

      Light.FdOn.Value := O3ds.Lights[I].Enabled;
      Light.FdLocation.Value := O3ds.Lights[I].Pos;
      Light.FdColor.Value := O3ds.Lights[I].Col;
    end;
  end;

  function MaterialToVRML(Material: TMaterial3ds): TNodeAppearance;
  var
    Mat: TNodeMaterial_2;
    Tex: TNodeImageTexture;
    TexTransform: TNodeTextureTransform;
  begin
    Result := TNodeAppearance.Create(MaterialVRMLName(Material.Name), WWWBasePath);

    Mat := TNodeMaterial_2.Create('', WWWBasePath);
    Mat.FdDiffuseColor.Value := Vector3SingleCut(Material.DiffuseColor);
    Mat.FdAmbientIntensity.Value := AmbientIntensity(Material.AmbientColor, Material.DiffuseColor);
    Mat.FdSpecularColor.Value := Vector3SingleCut(Material.SpecularColor);
    Mat.FdShininess.Value := Material.Shininess;
    Mat.FdTransparency.Value := Material.Transparency;
    Result.FdMaterial.Value := Mat;

    if Material.TextureMap1.Exists then
    begin
      Tex := TNodeImageTexture.Create('', WWWBasePath);
      Tex.FdUrl.Items.Add(SearchTextureFileName(WWWBasePath,
        Material.TextureMap1.MapFilename));
      Result.FdTexture.Value := Tex;

      TexTransform := TNodeTextureTransform.Create('', WWWBasePath);
      TexTransform.FdScale.Value := Material.TextureMap1.Scale;
      Result.FdTextureTransform.Value := TexTransform;

      if Material.TextureMapBump.Exists then
      begin
        Tex := TNodeImageTexture.Create('', WWWBasePath);
        Tex.FdUrl.Items.Add(SearchTextureFileName(WWWBasePath,
          Material.TextureMapBump.MapFilename));
        Result.FdNormalMap.Value := Tex;

        { We don't have separate TextureTransform for bump map.
          Just check that in 3DS bump map and diffuse textures have equal transform. }
        if not VectorsEqual(
            Material.TextureMap1.Scale,
            Material.TextureMapBump.Scale) then
          VRMLWarning(vwIgnorable, 'Texture scale for diffuse and normal (bump) maps is different in the 3DS file. Currently this is not correctly handled when converting to VRML/X3D');
      end;
    end;
  end;

  { How many faces have the same material index.
    Starts, and compares, with the face numnbered StartFace (must be < FacesCount). }
  function SameMaterialFacesCount(Faces: PArray_Face3ds; FacesCount: integer;
    StartFace: integer): integer;
  var
    I: Integer;
  begin
    I := StartFace + 1;
    while (I < FacesCount) and
          (Faces^[I].FaceMaterialIndex = Faces^[StartFace].FaceMaterialIndex) do
      Inc(i);
    Result := I - StartFace;
  end;

var
  Trimesh3ds: TTrimesh3ds;
  Appearances: TVRMLNodesList;
  Coord: TNodeCoordinate;
  IFS: TNodeIndexedFaceSet_2;
  TexCoord: TNodeTextureCoordinate;
  Shape: TNodeShape;
  I, J, FaceMaterialNum, ThisMaterialFacesCount, FaceNum: Integer;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(filename));
  Appearances := nil;
  O3ds := TScene3DS.Create(filename);
  try
    Result := TVRMLRootNode.Create('', WWWBasePath);
    try
      Result.HasForceVersion := true;
      Result.ForceVersion := VRML2Version;

      AddViewpoints;
      AddLights;

      { Convert every 3DS material into VRML/X3D Appearance node }
      Appearances := TVRMLNodesList.Create;
      Appearances.Count := O3ds.Materials.Count;
      for i := 0 to O3ds.Materials.Count - 1 do
        Appearances[I] := MaterialToVRML(O3ds.Materials[i]);

      { Add 3DS triangle meshes. Each trimesh is split into a number of
        VRML/X3D IndexedFaceSet nodes, sharing common Coordinate node,
        but having different materials. }
      for i := 0 to O3ds.Trimeshes.Count-1 do
      begin
        Trimesh3ds := O3ds.Trimeshes[I];

        { Create Coordinate node }
        Coord := TNodeCoordinate.Create('Coord_' + TrimeshVRMLName(Trimesh3ds.Name), WWWBasePath);
        Coord.FdPoint.Count := Trimesh3ds.VertsCount;
        for J := 0 to Trimesh3ds.VertsCount-1 do
          Coord.FdPoint.Items.Items[J] := Trimesh3ds.Verts^[J].Pos;

        { Create TextureCoordinate node, or nil if not available }
        if Trimesh3ds.HasTexCoords then
        begin
          TexCoord := TNodeTextureCoordinate.Create('TexCoord_' + TrimeshVRMLName(Trimesh3ds.Name), WWWBasePath);
          TexCoord.FdPoint.Count := Trimesh3ds.VertsCount;
          for j := 0 to Trimesh3ds.VertsCount - 1 do
            TexCoord.FdPoint.Items.Items[J] := Trimesh3ds.Verts^[J].TexCoord;
        end else
          TexCoord := nil;

        { Convert faces with equal material to IndexedFaceSet }
        J := 0;
        while J < Trimesh3ds.FacesCount do
        begin
          FaceMaterialNum := Trimesh3ds.Faces^[j].FaceMaterialIndex;

          IFS := TNodeIndexedFaceSet_2.Create('', WWWBasePath);
          IFS.FdTexCoord.Value := TexCoord;
          IFS.FdCoord.Value := Coord;
          { We don't support 3DS smoothing groups.
            So instead assign some sensible non-zero crease angle. }
          IFS.FdCreaseAngle.Value := NiceCreaseAngle;
          IFS.FdSolid.Value := false;

          Shape := TNodeShape.Create('', WWWBasePath);
          Shape.FdGeometry.Value := IFS;
          Shape.Appearance := TNodeAppearance(Appearances[FaceMaterialNum]);

          Result.FdChildren.Add(Shape);

          ThisMaterialFacesCount := SameMaterialFacesCount(Trimesh3ds.Faces,
            Trimesh3ds.FacesCount, J);

          IFS.FdCoordIndex.Count := ThisMaterialFacesCount * 4;
          for FaceNum := 0 to ThisMaterialFacesCount - 1 do
          begin
            with IFS.FdCoordIndex.Items do
            begin
              Items[FaceNum * 4    ] := Trimesh3ds.Faces^[J].VertsIndices[0];
              Items[FaceNum * 4 + 1] := Trimesh3ds.Faces^[J].VertsIndices[1];
              Items[FaceNum * 4 + 2] := Trimesh3ds.Faces^[J].VertsIndices[2];
              Items[FaceNum * 4 + 3] := -1;
            end;
            Inc(J);
          end;

          { If Trimesh3ds.HasTexCoords then IFS.texCoordIndex should be taken
            from IFS.coordIndex. And VRML 2.0/X3D do this by default, so no need
            to do anything. }
        end;

        Coord.FreeIfUnused;
        Coord := nil;
        if TexCoord <> nil then
        begin
          TexCoord.FreeIfUnused;
          TexCoord := nil;
        end;
      end;

      for I := 0 to Appearances.Count - 1 do
        Appearances[I].FreeIfUnused;
    except FreeAndNil(result); raise end;
  finally
    FreeAndNil(O3ds);
    FreeAndNil(Appearances)
  end;
end;

function LoadMD3Frame(Md3: TObject3DMD3; FrameNumber: Cardinal;
  const WWWBasePath: string): TVRMLRootNode;
var
  Texture: TNodeImageTexture;
  SceneBox: TBox3D;

  function MakeCoordinates(Vertexes: TDynMd3VertexArray;
    VertexesInFrameCount: Cardinal): TNodeCoordinate;
  var
    I: Integer;
    V: PMd3Vertex;
  begin
    Result := TNodeCoordinate.Create('', WWWBasePath);
    Result.FdPoint.Items.Count := VertexesInFrameCount;
    V := Vertexes.Pointers[VertexesInFrameCount * FrameNumber];
    for I := 0 to VertexesInFrameCount - 1 do
    begin
      Result.FdPoint.Items.Items[I] := Vector3Single(
        V^.Position[0] * Md3XyzScale,
        V^.Position[1] * Md3XyzScale,
        V^.Position[2] * Md3XyzScale);
      Inc(V);
    end;

    Box3DSumTo1st(SceneBox, CalculateBoundingBox(Result.FdPoint.Items));
  end;

  function MakeTextureCoordinates(
    TextureCoords: TDynMd3TexCoordArray): TNodeTextureCoordinate;
  var
    I: Integer;
    V: PMd3TexCoord;
  begin
    Result := TNodeTextureCoordinate.Create('', WWWBasePath);
    Result.FdPoint.Items.Count := TextureCoords.Count;
    V := TextureCoords.Pointers[0];
    for I := 0 to TextureCoords.Count - 1 do
    begin
      Result.FdPoint.Items.Items[I] := Vector2Single(V^.St[0], 1-V^.St[1]);
      Inc(V);
    end;
  end;

  function MakeIndexes(Triangles: TDynMd3TriangleArray): TNodeIndexedFaceSet_2;
  var
    I: Integer;
  begin
    Result := TNodeIndexedFaceSet_2.Create('', WWWBasePath);
    Result.FdCreaseAngle.Value := NiceCreaseAngle;
    Result.FdSolid.Value := false;
    Result.FdCoordIndex.Items.Count := Triangles.Count * 4;
    Result.FdTexCoordIndex.Items.Count := Triangles.Count * 4;
    for I := 0 to Triangles.Count - 1 do
    begin
      Result.FdCoordIndex.Items.Items[I*4 + 0] := Triangles.Items[I].Indexes[0];
      Result.FdCoordIndex.Items.Items[I*4 + 1] := Triangles.Items[I].Indexes[1];
      Result.FdCoordIndex.Items.Items[I*4 + 2] := Triangles.Items[I].Indexes[2];
      Result.FdCoordIndex.Items.Items[I*4 + 3] := -1;

      Result.FdTexCoordIndex.Items.Items[I*4 + 0] := Triangles.Items[I].Indexes[0];
      Result.FdTexCoordIndex.Items.Items[I*4 + 1] := Triangles.Items[I].Indexes[1];
      Result.FdTexCoordIndex.Items.Items[I*4 + 2] := Triangles.Items[I].Indexes[2];
      Result.FdTexCoordIndex.Items.Items[I*4 + 3] := -1;
    end;
  end;

  function MakeShape(Surface: TMd3Surface): TNodeShape;
  var
    IFS: TNodeIndexedFaceSet_2;
  begin
    IFS := MakeIndexes(Surface.Triangles);
    IFS.FdCoord.Value := MakeCoordinates(Surface.Vertexes, Surface.VertexesInFrameCount);
    IFS.FdTexCoord.Value := MakeTextureCoordinates(Surface.TextureCoords);

    Result := TNodeShape.Create(ToVRMLName(Surface.Name), WWWBasePath);
    Result.FdGeometry.Value := IFS;
    Result.Material := TNodeMaterial_2.Create('', WWWBasePath);
    Result.Texture := Texture;
  end;

var
  I: Integer;
begin
  Result := TVRMLRootNode.Create(
    ToVRMLName(Md3.Name
      { Although adding here FrameNumber is not a bad idea, but VRMLGLAnimation
        requires for now that sequence of VRML models have the same node names }
      { + '_Frame' + IntToStr(FrameNumber) }), WWWBasePath);

  Result.HasForceVersion := true;
  Result.ForceVersion := VRML2Version;

  SceneBox := EmptyBox3D;

  if Md3.TextureFileName <> '' then
  begin
    Texture := TNodeImageTexture.Create('', WWWBasePath);
    Texture.FdUrl.Items.Add(Md3.TextureFileName);
  end else
    Texture := nil;

  for I := 0 to Md3.Surfaces.Count - 1 do
    Result.FdChildren.Add(MakeShape(Md3.Surfaces[I]));

  { MD3 files have no camera. I add camera here, just to force GravityUp
    to be in +Z, since this is the convention used in all MD3 files that
    I saw (so I guess that Quake3 engine generally uses this convention). }
  Result.FdChildren.Add(CameraNodeForWholeScene(2, WWWBasePath, SceneBox,
    0, 2, false, true));

  if Texture <> nil then
  begin
    Texture.FreeIfUnused;
    Texture := nil;
  end;
end;

function LoadMD3(const FileName: string): TVRMLRootNode;
var
  Md3: TObject3DMD3;
  WWWBasePath: string;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(FileName));
  Md3 := TObject3DMD3.Create(FileName);
  try
    Result := LoadMD3Frame(Md3, 0, WWWBasePath);
  finally FreeAndNil(Md3) end;
end;

procedure LoadMD3Sequence(
  const FileName: string;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);
var
  Md3: TObject3DMD3;
  WWWBasePath: string;
  I: Integer;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(FileName));
  Md3 := TObject3DMD3.Create(FileName);
  try
    { handle each MD3 frame }
    for I := 0 to Md3.FramesCount - 1 do
    begin
      RootNodes.Add(LoadMD3Frame(Md3, I, WWWBasePath));
      Times.Add(I / 30);
    end;

    ScenesPerTime := 30;
    { Default ScenesPerTime and times are set such that one MD3
      frame will result in one frame inside TVRMLGLAnimation.
      So don't try to merge these frames (on the assumption that
      they are not merged in MD3... so hopefully there's no need for it ?). }
    EqualityEpsilon := 0.0;

    { Really, no sensible default for Loop/Backwards here...
      I set Loop to @false, otherwise it's not clear for user when
      animation ends. }
    TimeLoop := false;
    TimeBackwards := false;
  finally FreeAndNil(Md3) end;
end;

function LoadVRML(const filename: string;
  AllowStdIn: boolean): TVRMLRootNode;
const
  GzExt = '.gz';
  Extensions: array [0..14] of string =
  ('.geo', '.3ds', '.obj',
   '.iv',
   '.wrl', '.wrl' + GzExt, '.wrz',
   '.x3dv', '.x3dv' + GzExt, '.x3dvz',
   '.md3', '.dae',
   '.x3d', '.x3dz', '.x3d' + GzExt);
var
  Ext: string;
begin
  if AllowStdIn and (FileName = '-') then
    result := LoadVRMLClassic('-', true) else
  begin
    Ext := ExtractFileExt(filename);
    if Ext = '.gz' then
      Ext := ExtractFileExt(DeleteFileExt(FileName)) + Ext;
    case ArrayPosText(Ext, Extensions) of
      0: result := LoadGEO(filename);
      1: result := Load3DS(filename);
      2: result := LoadWavefrontOBJ(filename);
      3..9: result := LoadVRMLClassic(filename, false);
      10: Result := LoadMD3(FileName);
      11: Result := LoadCollada(FileName);
      12: Result := LoadX3DXml(FileName, false);
      13, 14: Result := LoadX3DXml(FileName, true);
      else raise Exception.CreateFmt(
        'Unrecognized file extension "%s" for 3D model file "%s"',
        [Ext, FileName]);
    end;
  end;
end;

function LoadAsVRML(const filename: string;
  AllowStdIn: boolean): TVRMLRootNode;
begin
  Result := LoadVRML(FileName, AllowStdIn);
end;

procedure LoadVRMLSequence(const FileName: string;
  AllowStdIn: boolean;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

  procedure LoadKanim;
  var
    ModelFileNames: TDynStringArray;
    I, J: Integer;
  begin
    ModelFileNames := TDynStringArray.Create;
    try
      TVRMLAnimation.LoadFromFileToVars(FileName, ModelFileNames, Times,
        ScenesPerTime, EqualityEpsilon, TimeLoop, TimeBackwards);

      Assert(ModelFileNames.Length = Times.Length);
      Assert(ModelFileNames.Length >= 1);

      { Now use ModelFileNames to load RootNodes }
      RootNodes.Count := ModelFileNames.Count;
      for I := 0 to ModelFileNames.High do
      try
        RootNodes[I] := LoadVRML(ModelFileNames[I]);
      except
        for J := 0 to I - 1 do
          RootNodes.FreeAndNil(J);
        raise;
      end;
    finally FreeAndNil(ModelFileNames) end;
  end;

  procedure LoadSingle(Node: TVRMLNode);
  begin
    RootNodes.Add(Node);
    Times.Add(0); { One time value }
    ScenesPerTime := 1;      { doesn't matter }
    EqualityEpsilon := 0.0;  { doesn't matter }
    TimeLoop := false;      { doesn't matter }
    TimeBackwards := false; { doesn't matter }
  end;

var
  Ext: string;
begin
  Assert(Times.Length = 0);
  Assert(RootNodes.Count = 0);

  Ext := ExtractFileExt(FileName);
  if SameText(Ext, '.kanim') then
    LoadKanim else
  if SameText(Ext, '.md3') then
    LoadMD3Sequence(FileName, RootNodes, Times, ScenesPerTime,
      EqualityEpsilon, TimeLoop, TimeBackwards) else
    LoadSingle(LoadVRML(FileName, AllowStdIn));
end;

end.
