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

uses Object3DGEO, Object3Ds, Object3DOBJ, VRMLCameraUtils,
  KambiStringUtils, VRMLAnimation, ColladaToVRML;

{
  Note: for VRML 1.0, remember that you may want to embed returned
  @link(TVRMLNode) objects inside VRML Separator node before
  inserting it into some existing VRML scene.
}

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

function FileNameToVRMLName(const filename: string): string;
begin  result := 'File_' + ToVRMLName(filename)  end;

{ Load* ---------------------------------------------------------------------- }

function LoadGEO(const filename: string): TVRMLRootNode;
var geo: TObject3DGEO;
    verts: TNodeCoordinate3;
    faces: TNodeIndexedFaceSet_1;
    i: integer;
    WWWBasePath: string;
begin
 WWWBasePath := ExtractFilePath(ExpandFilename(filename));
 geo := TObject3DGEO.Create(filename);
 try
  result := TVRMLRootNode.Create(FileNameToVRMLName(filename), WWWBasePath);
  try
   Result.ForceVersion := true;
   Result.ForceVersionMajor := 1;
   Result.ForceVersionMinor := 0;

   verts := TNodeCoordinate3.Create('', WWWBasePath);
   result.VRML1ChildAdd(verts);
   faces := TNodeIndexedFaceSet_1.Create('', WWWBasePath);
   result.VRML1ChildAdd(faces);

   verts.FdPoint.Items.SetLength(0);
   verts.FdPoint.Items.AppendDynArray(geo.Verts);

   faces.FdCoordIndex.Items.SetLength(geo.Faces.Count*4);
   for i := 0 to geo.Faces.Count-1 do
   begin
    faces.FdCoordIndex.Items[i*4] := geo.Faces.Items[i][0];
    faces.FdCoordIndex.Items[i*4+1] := geo.Faces.Items[i][1];
    faces.FdCoordIndex.Items[i*4+2] := geo.Faces.Items[i][2];
    faces.FdCoordIndex.Items[i*4+3] := -1;
   end;
  except result.Free; raise end;
 finally geo.Free end;
end;

function LoadWavefrontOBJ(const filename: string): TVRMLRootNode;
const
  { na czas konstruowania duzych tablic indeksow pozwalamy sobie ustawiac
    bardzo duze dopuszczalne AllowedCapacityOverflow zeby wszystko bylo szybko.

    TODO: powinienes unikac uzywania tego, zrob tu tak jak w Load3DS.}
  ALLOWED_INDICES_ARRAYS_OVERFLOWS = 100;

  function MatOBJNameToVRMLName(const MatOBJName: string): string;
  begin
    Result := 'Material_' + ToVRMLName(MatOBJName);
  end;

var
  obj: TObject3DOBJ;
  verts: TNodeCoordinate3;
  faces: TNodeIndexedFaceSet_1;
  texcoords: TNodeTextureCoordinate2;
  texture: TNodeTexture2;
  i: integer;
  fourIndices: array[0..3]of Longint;
  FacesWithTexCoords, FacesWithNormals: boolean;
  WWWBasePath: string;
  Normals: TNodeNormal;
  FacesWithMaterial: TWavefrontMaterial;
  MaterialsSwitch: TNodeSwitch_1;
  MaterialGroup: TNodeGroup_1;
  Material: TNodeMaterial_1;
  FacesSeparator: TNodeSeparator;
begin
 WWWBasePath := ExtractFilePath(ExpandFilename(filename));
 obj := TObject3DOBJ.Create(filename);
 try
  result := TVRMLRootNode.Create(''
    { I used to put here FileNameToVRMLName(filename), but
      it made two OBJ models structurally not equal, so demo_animation
      couldn't animate them. Conceptually, you can say that OBJ filename
      shouldn't be recorded as VRML field name, since filename is something
      not related to actual *content* of the model. },
    WWWBasePath);
  try
   Result.ForceVersion := true;
   Result.ForceVersionMajor := 1;
   Result.ForceVersionMinor := 0;

   MaterialsSwitch := TNodeSwitch_1.Create('Materials', WWWBasePath);
   Result.VRML1ChildAdd(MaterialsSwitch);

   for I := 0 to Obj.Materials.Count - 1 do
   begin
     MaterialGroup := TNodeGroup_1.Create(
       MatOBJNameToVRMLName(Obj.Materials.Items[I].Name), WWWBasePath);
     MaterialsSwitch.VRML1ChildAdd(MaterialGroup);

     Material := TNodeMaterial_1.Create('', WWWBasePath);
     MaterialGroup.VRML1ChildAdd(Material);
     Material.FdAmbientColor.Items.SetLength(1);
     Material.FdAmbientColor.Items.Items[0] := Obj.Materials.Items[I].AmbientColor;
     Material.FdDiffuseColor.Items.SetLength(1);
     Material.FdDiffuseColor.Items.Items[0] := Obj.Materials.Items[I].DiffuseColor;
     Material.FdSpecularColor.Items.SetLength(1);
     Material.FdSpecularColor.Items.Items[0] := Obj.Materials.Items[I].SpecularColor;
     Material.FdTransparency.Items.SetLength(1);
     Material.FdTransparency.Items.Items[0] := 1 - Obj.Materials.Items[I].Opacity;
     Material.FdShininess.Items.SetLength(1);
     Material.FdShininess.Items.Items[0] :=
       Obj.Materials.Items[I].SpecularExponent / 128.0;

     Texture := TNodeTexture2.Create('', WWWBasePath);
     MaterialGroup.VRML1ChildAdd(Texture);
     Texture.FdFilename.Value := Obj.Materials.Items[I].DiffuseTextureFileName;
   end;

   verts := TNodeCoordinate3.Create('',WWWBasePath);
   result.VRML1ChildAdd(verts);
   verts.FdPoint.Items.SetLength(0);
   verts.FdPoint.Items.AppendDynArray(obj.Verts);

   texcoords := TNodeTextureCoordinate2.Create('', WWWBasePath);
   result.VRML1ChildAdd(texcoords);
   texcoords.FdPoint.Items.SetLength(0);
   texcoords.FdPoint.Items.AppendDynArray(obj.TexCoords);

   Normals := TNodeNormal.Create('', WWWBasePath);
   Result.VRML1ChildAdd(Normals);
   Normals.FdVector.Items.SetLength(0);
   Normals.FdVector.Items.AppendDynArray(Obj.Normals);

   i := 0;
   while i < obj.Faces.Count do
   begin
    FacesWithTexCoords := Obj.Faces.Items[i].HasTexCoords;
    FacesWithNormals := Obj.Faces.Items[i].HasNormals;
    FacesWithMaterial := Obj.Faces.Items[i].Material;

    FacesSeparator := TNodeSeparator.Create('', WWWBasePath);
    Result.VRML1ChildAdd(FacesSeparator);

    if FacesWithMaterial <> nil then
    begin
      { We use material from the MaterialsSwitch.
        We find it by name, using FindNodeByName, we're sure that we will
        find this material --- since we added all materials to
        MaterialsSwitch. }
      FacesSeparator.VRML1ChildAdd(MaterialsSwitch.FindNodeByName(TVRMLNode,
        MatOBJNameToVRMLName(FacesWithMaterial.Name),
        false));
    end;

    (* else
    if FacesWithTexCoords then
    begin
      { if no material specified, but FacesWithTexCoords, we used to insert
        simple Texture2 node that uses
        OBJModelTextureFilename = 'default_obj_texture.png'.

        I removed this later, as texture filename may be specified (we read
        material file for OBJ), and blender exporter can even write faces
        with texture coordinates for objects without materials, so no texture
        also. IOW, it seems more sensible to not output Texture node if no
        material is present. (still, we output textureCoordIndex to VRML in
        this case, it will not be used, but user has the possibility to add
        Texture2 node by hand then, and he will have ready texture coords). }

      Texture := TNodeTexture2.Create('',WWWBasePath);
      FacesSeparator.VRML1ChildAdd(texture);
      Texture.FdFilename.Value := OBJModelTextureFilename;
    end;
    *)

    faces := TNodeIndexedFaceSet_1.Create('',WWWBasePath);
    FacesSeparator.VRML1ChildAdd(faces);
    faces.FdCoordIndex.Items.SetLength(0);
    faces.FdCoordIndex.Items.AllowedCapacityOverflow := ALLOWED_INDICES_ARRAYS_OVERFLOWS;
    faces.FdTextureCoordIndex.Items.SetLength(0);
    faces.FdTextureCoordIndex.Items.AllowedCapacityOverflow := ALLOWED_INDICES_ARRAYS_OVERFLOWS;
    faces.FdNormalIndex.Items.SetLength(0);
    faces.FdNormalIndex.Items.AllowedCapacityOverflow := ALLOWED_INDICES_ARRAYS_OVERFLOWS;

    { We add Faces as long as FacesWithXxx parameters stay the same.
      We know that at least the next face is Ok. }
    repeat
     fourIndices[0] := obj.Faces.Items[i].VertIndices[0];
     fourIndices[1] := obj.Faces.Items[i].VertIndices[1];
     fourIndices[2] := obj.Faces.Items[i].VertIndices[2];
     fourIndices[3] := -1;
     faces.FdCoordIndex.Items.AppendArray(fourIndices);

     if FacesWithTexCoords then
     begin
      fourIndices[0] := obj.Faces.Items[i].TexCoordIndices[0];
      fourIndices[1] := obj.Faces.Items[i].TexCoordIndices[1];
      fourIndices[2] := obj.Faces.Items[i].TexCoordIndices[2];
      fourIndices[3] := -1;
      faces.FdTextureCoordIndex.Items.AppendArray(fourIndices);
     end;

     if FacesWithNormals then
     begin
      fourIndices[0] := obj.Faces.Items[i].NormalIndices[0];
      fourIndices[1] := obj.Faces.Items[i].NormalIndices[1];
      fourIndices[2] := obj.Faces.Items[i].NormalIndices[2];
      fourIndices[3] := -1;
      faces.FdNormalIndex.Items.AppendArray(fourIndices);
     end;

     Inc(i);
    until (i >= obj.Faces.Count) or
      (FacesWithTexCoords <> obj.Faces.Items[i].HasTexCoords) or
      (FacesWithNormals <> obj.Faces.Items[i].HasNormals) or
      (FacesWithMaterial <> obj.Faces.Items[i].Material);

    faces.FdCoordIndex.Items.AllowedCapacityOverflow := 4;
    faces.FdTextureCoordIndex.Items.AllowedCapacityOverflow := 4;
   end;
  except result.Free; raise end;
 finally obj.Free end;
end;

function Load3DS(const filename: string): TVRMLRootNode;
var WWWBasePath: string;

  const
    { ta stala byc moze zostanie kiedys przeniesiona na interfejsu jakiegos
      ogolnego modulu VRMLa zeby view3dscene mogl oblugiwac wiele
      kamer zdefiniowanych w plik VRMLa. Nazwa 'Viewports' jest tutaj
      pewnym de facto standardem uzywanym przez wiele narzedzi generujacych
      VRMLe. }
    CamerasSwitchName = 'Viewpoints';

  { poprzedzamy nazwy materialow, trimeshy i camer prefixami rodzajowymi
    (np. 'Material_' dla materialow) zeby zapewnic ze te nazwy
    beda jakby w osobnych namespace i nie beda ze soba kolidowac.

    Zwracam tu uwage ze nie musimy sie tu martwic o to zeby wszystkie
    nazwy byly na pewno unikalne - przeciez w VRMLu nazwy nie musza byc
    unikalne. Tym niemniej uzywajac mozliwie unikalnych nazw pozwolimy
    sobie uzywac mozliwie czesto konstrukcji DEF/USE VRMLa przy zapisie
    modeli do pliku, a to jest zdecydowanie pozadane. }
  function Mat3dsNameToVRMLName(const Mat3dsName: string): string;
  begin  result := 'Material_' + ToVRMLName(Mat3dsName)  end;

  function Trimesh3dsNameToVRMLName(const Tri3dsName: string): string;
  begin  result := 'Trimesh_' + ToVRMLName(Tri3dsName)  end;

  function Camera3dsNameToVRMLName(const Camera3dsName: string): string;
  begin  result := 'Camera_' + ToVRMLName(Camera3dsName)  end;

  function Light3dsNameToVRMLName(const Light3dsName: string): string;
  begin  result := 'Light_' + ToVRMLName(Light3dsName)  end;


  function SameMaterialFacesCount(Faces: PArray_Face3ds; FacesCount: integer;
    StartFace: integer): integer;
  { zwraca ile jest w tablicy Faces elementow o FaceMaterialIndex =
    Faces[StartFace].FaceMaterialIndex zaczynajac liczyc od StartFace
    do pierwszego face ktory ma inny FaceMaterialIndex.
    Jezeli zwroci np. 3 to znaczy ze
      Faces[StartFace].FaceMaterialIndex =
      Faces[StartFace+1].FaceMaterialIndex =
      Faces[StartFace+2].FaceMaterialIndex <>
      Faces[StartFace+3].FaceMaterialIndex
    Pilnuje sie zeby nie wyjsc za zakres FacesCount, to znaczy jest tak jakby
    na koncu tablicy Faces element o indeksie FacesCount mial FaceMaterialIndex
    ktory na pewno jest rozny od kazdego FaceMaterialIndex.
    Podane StartFace musi byc < od FacesCount. }
  var i: integer;
  begin
   i := StartFace+1;
   while (i < FacesCount) and (Faces^[i].FaceMaterialIndex =
     Faces^[StartFace].FaceMaterialIndex) do
    Inc(i);
   result := i-StartFace;
  end;

  procedure Add3dsCameras(scene: TScene3ds; node: TVRMLNode);
  var camSwitch: TNodeSwitch_1;
      camera: TVRMLNode;
      i: Integer;
  begin
   if scene.Cameras.Count = 0 then Exit;

   camSwitch := TNodeSwitch_1.Create(CamerasSwitchName, WWWBasePath);
   node.VRML1ChildAdd(camSwitch);
   camSwitch.FdWhichChild.Value := 0;

   for i := 0 to scene.Cameras.Count-1 do
   begin
    camera := MakeVRMLCameraNode(1, WWWBasePath,
      scene.Cameras[i].CamPos,
      scene.Cameras[i].CamDir,
      scene.Cameras[i].CamUp,
      scene.Cameras[i].CamUp { GravityUp equals CamUp });
    camera.NodeName := Camera3dsNameToVRMLName(scene.Cameras[i].Name);
    camSwitch.VRML1ChildAdd(camera);

    { TODO: wykorzystac pozostale pola kamery 3ds }
   end;
  end;

  procedure Add3dsLights(scene: TScene3ds; node: TVRMLNode);
  var i: Integer;
      light: TNodePointLight_1;
      lightGroup: TNodeGroup_1;
  begin
   if Scene.Lights.Count = 0 then Exit;

   lightGroup := TNodeGroup_1.Create('Lights', WWWBasePath);
   node.VRML1ChildAdd(lightGroup);

   for i := 0 to Scene.Lights.Count-1 do
   begin
    light := TNodePointLight_1.Create(Light3dsNameToVRMLName(
      Scene.Lights[i].Name), WWWBasePath);
    lightGroup.VRML1ChildAdd(light);

    light.FdOn.Value := Scene.Lights[i].Enabled;
    light.FdLocation.Value := Scene.Lights[i].Pos;
    light.FdColor.Value := Scene.Lights[i].Col;
   end;
  end;

var obj3ds: TScene3ds;
    trimesh3ds: TTrimesh3ds;

    materialSwitch: TNodeSwitch_1;
    materialGroup, trimeshGroup: TNodeGroup_1;
    indexedFacesNode: TNodeIndexedFaceSet_1;
    trimeshCoords: TNodeCoordinate3;
    trimeshTexCoords: TNodeTextureCoordinate2;
    facesSep: TNodeSeparator;

    tmp: TVRMLNode;
    i, j, FaceMaterialNum, ThisMaterialFacesCount, FaceNum: integer;
begin
 WWWBasePath := ExtractFilePath(ExpandFilename(filename));
 obj3ds := TScene3ds.Create(filename);
 try
  result := TVRMLRootNode.Create(FileNameToVRMLName(filename), WWWBasePath);
  try
   Result.ForceVersion := true;
   Result.ForceVersionMajor := 1;
   Result.ForceVersionMinor := 0;

   Add3dsCameras(obj3ds, result);
   Add3dsLights(obj3ds, result);

   { konstruuj liste materiali jako dzieci materialSwitch }
   materialSwitch := TNodeSwitch_1.Create('Materials', WWWBasePath);
   result.VRML1ChildAdd(materialSwitch);

   for i := 0 to obj3ds.Materials.Count-1 do
   begin
    materialGroup := TNodeGroup_1.Create(Mat3dsNameToVRMLName(obj3ds.Materials[i].Name), WWWBasePath);
    materialSwitch.VRML1ChildAdd(materialGroup);

    { dodaj Material node }
    tmp := TNodeMaterial_1.Create('', WWWBasePath);
    materialGroup.VRML1ChildAdd(tmp);
    TNodeMaterial_1(tmp).FdAmbientColor.Items.SetLength(1);
    TNodeMaterial_1(tmp).FdAmbientColor.Items.Items[0] := Vector3SingleCut(obj3ds.Materials[i].AmbientCol);
    TNodeMaterial_1(tmp).FdDiffuseColor.Items.SetLength(1);
    TNodeMaterial_1(tmp).FdDiffuseColor.Items.Items[0] := Vector3SingleCut(obj3ds.Materials[i].DiffuseCol);
    TNodeMaterial_1(tmp).FdSpecularColor.Items.SetLength(1);
    TNodeMaterial_1(tmp).FdSpecularColor.Items.Items[0] := Vector3SingleCut(obj3ds.Materials[i].SpecularCol);
    TNodeMaterial_1(tmp).FdShininess.Items.SetLength(1);
    TNodeMaterial_1(tmp).FdShininess.Items.Items[0] := obj3ds.Materials[i].Shininess;
    TNodeMaterial_1(tmp).FdTransparency.Items.SetLength(1);
    TNodeMaterial_1(tmp).FdTransparency.Items.Items[0] := obj3ds.Materials[i].Transparency;

    if obj3ds.Materials[i].TextureMap1.Exists then
    begin
     { dodaj Texture2 i Texture2Transform nodes }
     tmp := TNodeTexture2.Create('', WWWBasePath);
     materialGroup.VRML1ChildAdd(tmp);
     TNodeTexture2(tmp).FdFilename.Value := obj3ds.Materials[i].TextureMap1.MapFilename;

     tmp := TNodeTexture2Transform.Create('', WWWBasePath);
     materialGroup.VRML1ChildAdd(tmp);
     TNodeTexture2Transform(tmp).FdScaleFactor.Value :=
       Vector2Single(obj3ds.Materials[i].TextureMap1.UScale,
                     obj3ds.Materials[i].TextureMap1.VScale);
    end;
   end;

   { teraz dodawaj trimeshes. Kazde trimesh to jeden node Group w srodku
     ktorego sa Coordinate3, potem ciagi Faces - kazdy ciag w Separatorze,
     kazdy taki Separator to najpierw jakis material, potem IndexedFaceSet. }
   for i := 0 to obj3ds.Trimeshes.Count-1 do
   begin
    trimesh3ds := obj3ds.Trimeshes[i];

    trimeshGroup := TNodeGroup_1.Create(Trimesh3dsNameToVRMLName(trimesh3ds.Name), WWWBasePath);
    result.VRML1ChildAdd(trimeshGroup);

    { zapisz Coordinate3 }
    trimeshCoords := TNodeCoordinate3.Create('', WWWBasePath);
    trimeshGroup.VRML1ChildAdd(trimeshCoords);
    trimeshCoords.FdPoint.Items.SetLength(trimesh3ds.VertsCount);
    for j := 0 to trimesh3ds.VertsCount-1 do
     trimeshCoords.FdPoint.Items.Items[j] := trimesh3ds.Verts^[j].Pos;

    { zapisz TextureCoordinate2 jesli je mamy }
    if trimesh3ds.HasTexCoords then
    begin
     trimeshTexCoords := TNodeTextureCoordinate2.Create('', WWWBasePath);
     trimeshGroup.VRML1ChildAdd(trimeshTexCoords);
     trimeshTexCoords.FdPoint.Items.SetLength(trimesh3ds.VertsCount);
     for j := 0 to trimesh3ds.VertsCount-1 do
      trimeshTexCoords.FdPoint.Items.Items[j] := trimesh3ds.Verts^[j].TexCoord;
    end;

    { zapisz faces }
    j := 0;
    while j < trimesh3ds.FacesCount do
    begin
     FaceMaterialNum := trimesh3ds.Faces^[j].FaceMaterialIndex;
     facesSep := TNodeSeparator.Create('', WWWBasePath);
     trimeshGroup.VRML1ChildAdd(facesSep);

     { uzyj materialu }
     if FaceMaterialNum >= 0 then
      facesSep.VRML1ChildAdd(materialSwitch.VRML1Children[FaceMaterialNum]);
      {else uzyje defaultowego materiala. I o to chodzi.}

     { wylacz teksture jesli material ma teksture ale my nie mamy texCoords }
     if (FaceMaterialNum >= 0) and
        (obj3ds.Materials[FaceMaterialNum].TextureMap1.Exists) and
        (not trimesh3ds.HasTexCoords) then
      facesSep.VRML1ChildAdd(TNodeTexture2.Create('Turn_Texture_Off', WWWBasePath));

     { zapisz faces o tym samym FaceMaterialNum.
       Mamy przynajmniej jedno takie face. }

     ThisMaterialFacesCount := SameMaterialFacesCount(trimesh3ds.Faces,
       trimesh3ds.FacesCount, j);

     indexedFacesNode := TNodeIndexedFaceSet_1.Create('', WWWBasePath);
     facesSep.VRML1ChildAdd(indexedFacesNode);
     indexedFacesNode.FdCoordIndex.Items.SetLength(ThisMaterialFacesCount*4);

     for FaceNum := 0 to ThisMaterialFacesCount-1 do
     begin
      with indexedFacesNode.FdCoordIndex.Items do
      begin
       Items[FaceNum*4  ] := trimesh3ds.Faces^[j].VertsIndices[0];
       Items[FaceNum*4+1] := trimesh3ds.Faces^[j].VertsIndices[1];
       Items[FaceNum*4+2] := trimesh3ds.Faces^[j].VertsIndices[2];
       Items[FaceNum*4+3] := -1;
      end;
      Inc(j);
     end;

     { jezeli trimesh3ds.HasTexCoords to textureCoordIndex sa takie same jako
       coordIndex }
     indexedFacesNode.FdTextureCoordIndex.Items.SetLength(0);
     if trimesh3ds.HasTexCoords then
      indexedFacesNode.FdTextureCoordIndex.Items.AppendDynArray(
        indexedFacesNode.FdCoordIndex.Items);
    end;

   end;
  except result.Free; raise end;
 finally obj3ds.Free end;
end;

function LoadMD3Frame(Md3: TObject3DMD3; FrameNumber: Cardinal;
  const WWWBasePath: string): TVRMLRootNode;

  function MakeCoordinates(Vertexes: TDynMd3VertexArray;
    VertexesInFrameCount: Cardinal): TNodeCoordinate3;
  var
    I: Integer;
    V: PMd3Vertex;
  begin
    Result := TNodeCoordinate3.Create('', WWWBasePath);
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
  end;

  function MakeTextureCoordinates(
    TextureCoords: TDynMd3TexCoordArray): TNodeTextureCoordinate2;
  var
    I: Integer;
    V: PMd3TexCoord;
  begin
    Result := TNodeTextureCoordinate2.Create('', WWWBasePath);
    Result.FdPoint.Items.Count := TextureCoords.Count;
    V := TextureCoords.Pointers[0];
    for I := 0 to TextureCoords.Count - 1 do
    begin
      Result.FdPoint.Items.Items[I] := Vector2Single(V^.St[0], 1-V^.St[1]);
      Inc(V);
    end;
  end;

  function MakeIndexes(Triangles: TDynMd3TriangleArray): TNodeIndexedFaceSet_1;
  var
    I: Integer;
  begin
    Result := TNodeIndexedFaceSet_1.Create('', WWWBasePath);
    Result.FdCoordIndex.Items.Count := Triangles.Count * 4;
    Result.FdTextureCoordIndex.Items.Count := Triangles.Count * 4;
    for I := 0 to Triangles.Count - 1 do
    begin
      Result.FdCoordIndex.Items.Items[I*4 + 0] := Triangles.Items[I].Indexes[0];
      Result.FdCoordIndex.Items.Items[I*4 + 1] := Triangles.Items[I].Indexes[1];
      Result.FdCoordIndex.Items.Items[I*4 + 2] := Triangles.Items[I].Indexes[2];
      Result.FdCoordIndex.Items.Items[I*4 + 3] := -1;

      Result.FdTextureCoordIndex.Items.Items[I*4 + 0] := Triangles.Items[I].Indexes[0];
      Result.FdTextureCoordIndex.Items.Items[I*4 + 1] := Triangles.Items[I].Indexes[1];
      Result.FdTextureCoordIndex.Items.Items[I*4 + 2] := Triangles.Items[I].Indexes[2];
      Result.FdTextureCoordIndex.Items.Items[I*4 + 3] := -1;
    end;
  end;

  function MakeSeparator(Surface: TMd3Surface): TNodeSeparator;
  begin
    Result := TNodeSeparator.Create(ToVRMLName(Surface.Name), WWWBasePath);
    Result.VRML1ChildAdd(MakeTextureCoordinates(Surface.TextureCoords));
    Result.VRML1ChildAdd(MakeCoordinates(Surface.Vertexes, Surface.VertexesInFrameCount));
    Result.VRML1ChildAdd(MakeIndexes(Surface.Triangles));
  end;

var
  I: Integer;
  Texture: TNodeTexture2;
begin
  Result := TVRMLRootNode.Create(
    ToVRMLName(Md3.Name
      { Although adding here FrameNumber is not a bad idea, but VRMLGLAnimation
        requires for now that sequence of VRML models have the same node names }
      { + '_Frame' + IntToStr(FrameNumber) }), WWWBasePath);

  Result.ForceVersion := true;
  Result.ForceVersionMajor := 1;
  Result.ForceVersionMinor := 0;

  { MD3 files have no camera. I add camera here, just to force GravityUp
    to be in +Z, since this is the convention used in all MD3 file that
    I saw (so I guess that Quake3 engine generally uses this convention). }
  Result.VRML1ChildAdd(MakeVRMLCameraNode(1, WWWBasePath,
    Vector3Single(0, 0, 0),
    Vector3Single(1, 0, 0),
    Vector3Single(0, 0, 1),
    Vector3Single(0, 0, 1)));

  if Md3.TextureFileName <> '' then
  begin
    Texture := TNodeTexture2.Create('', WWWBasePath);
    Result.VRML1ChildAdd(Texture);
    Texture.FdFilename.Value := Md3.TextureFileName;
  end;

  for I := 0 to Md3.Surfaces.Count - 1 do
    Result.VRML1ChildAdd(MakeSeparator(Md3.Surfaces[I]));
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
