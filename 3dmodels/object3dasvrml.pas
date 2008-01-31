{
  Copyright 2003-2008 Michalis Kamburelis.

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

  ----------------------------------------------------------------------------
}

{ @abstract(Converting 3D models to VRML.
  Converts models in various formats (3DS, MD3 etc.)
  to one or more (static or animation) VRML nodes.)

  Note: for VRML 1.0, remember that you may want to embed returned
  @link(TVRMLNode) objects inside VRML Separator node before
  inserting it into some existing VRML scene. }

unit Object3dAsVRML;

interface

uses VectorMath, SysUtils, VRMLNodes, VRMLFields, Boxes3d, Object3dMD3,
  KambiUtils, VRMLRendererOptimization;

function LoadGEOAsVRML(const filename: string): TVRMLNode;

function LoadOBJAsVRML(const filename: string): TVRMLNode;

function Load3dsAsVRML(const filename: string): TVRMLNode;

function LoadMD3AsVRML(const FileName: string): TVRMLNode;

{ Load a specific animation frame from a given Md3 model.
  @param Md3 is the MD3 file to use.
  @param FrameNumber is the frame number to load, must be < Md3.Count.
  @param WWBasePath is WWBasePath value to set in resulting VRML nodes. }
function LoadMD3FrameAsVRML(Md3: TObject3dMD3; FrameNumber: Cardinal;
  const WWWBasePath: string): TVRMLNode;

{ This is much like LoadAsVRMLSequence, but it only handles MD3 files.
  Usually you want to use LoadAsVRMLSequence, not this procedure. }
procedure LoadMD3AsVRMLSequence(
  const FileName: string;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  var Optimization: TGLRendererOptimization;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

function LoadColladaAsVRML(const FileName: string): TVRMLNode;

{ This guesses model format basing on ExtractFileExt(filename),
  then loads model converting it to VRML with appropriate
  LoadXxxAsVRML functions above in this unit or using
  @link(ParseVRMLFile) if this seems to be a VRML file.

  If AllowStdIn and FileName = '-' then it will load a VRML file
  from StdInStream (using GetCurrentDir as WWWBasePath). }
function LoadAsVRML(const filename: string; AllowStdIn: boolean = false): TVRMLNode;

{ Load various model formats as animation expressed by VRML sequence.

  For model formats that cannot express animations (like GEO or Wavefront OBJ)
  or that express animations in a single VRML file (like VRML > 2.0)
  this just loads them like LoadAsVRML, adding exactly one item
  to RootNodes.
  This guarantees that this function handles @italic(at least)
  the same model formats as LoadAsVRML --- but actually it may
  handle more.

  And indeed, it currently handles kanim, that
  is completely unrecognized by LoadAsVRML.

  This handles animations in kanim and MD3 formats.

  @param(RootNodes Sequence of root nodes will be stored there.
    Pass here some created and empty instance of TVRMLNodesList.)

  @param(ATimes Sequence of time values.
    Pass here some created and empty instance of TDynSingleArray.)

  @param(AOptimization This is a @code(var) variable: it will
    be set only if a file format will actually specify it
    (for now, this is only for kanim format).
    For other formats, it will not be modified.
    So you should set this to "preferred Optimization value for
    other formats than kanim".)
}
procedure LoadAsVRMLSequence(
  const FileName: string; AllowStdIn: boolean;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  var Optimization: TGLRendererOptimization;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);

implementation

uses Object3dGEO, Object3ds, Object3dOBJ, VRMLCameraUtils,
  KambiStringUtils, VRMLAnimation, DOM, XMLRead, KambiXMLUtils,
  DataErrors, Classes, KambiClassUtils;

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

function LoadGEOAsVRML(const filename: string): TVRMLNode;
var geo: TObject3dGEO;
    verts: TNodeCoordinate3;
    faces: TNodeIndexedFaceSet_1;
    i: integer;
    WWWBasePath: string;
begin
 WWWBasePath := ExtractFilePath(ExpandFilename(filename));
 geo := TObject3dGEO.Create(filename);
 try
  result := TNodeGroup_1.Create(FileNameToVRMLName(filename), WWWBasePath);
  try
   verts := TNodeCoordinate3.Create('',WWWBasePath);
   result.AddChild(verts);
   faces := TNodeIndexedFaceSet_1.Create('',WWWBasePath);
   result.AddChild(faces);

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

function LoadOBJAsVRML(const filename: string): TVRMLNode;
const
  { na czas konstruowania duzych tablic indeksow pozwalamy sobie ustawiac
    bardzo duze dopuszczalne AllowedCapacityOverflow zeby wszystko bylo szybko.

    TODO: powinienes unikac uzywania tego, zrob tu tak jak w Load3dsAsVRML.}
  ALLOWED_INDICES_ARRAYS_OVERFLOWS = 100;

  function MatOBJNameToVRMLName(const MatOBJName: string): string;
  begin
    Result := 'Material_' + ToVRMLName(MatOBJName);
  end;

var
  obj: TObject3dOBJ;
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
 obj := TObject3dOBJ.Create(filename);
 try
  result := TNodeGroup_1.Create(''
    { I used to put here FileNameToVRMLName(filename), but
      it made two OBJ models structurally not equal, so demo_animation
      couldn't animate them. Conceptually, you can say that OBJ filename
      shouldn't be recorded as VRML field name, since filename is something
      not related to actual *content* of the model. },
    WWWBasePath);
  try
   MaterialsSwitch := TNodeSwitch_1.Create('Materials', WWWBasePath);
   Result.AddChild(MaterialsSwitch);

   for I := 0 to Obj.Materials.Count - 1 do
   begin
     MaterialGroup := TNodeGroup_1.Create(
       MatOBJNameToVRMLName(Obj.Materials.Items[I].Name), WWWBasePath);
     MaterialsSwitch.AddChild(MaterialGroup);

     Material := TNodeMaterial_1.Create('', WWWBasePath);
     MaterialGroup.AddChild(Material);
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
     MaterialGroup.AddChild(Texture);
     Texture.FdFilename.Value := Obj.Materials.Items[I].DiffuseTextureFileName;
   end;

   verts := TNodeCoordinate3.Create('',WWWBasePath);
   result.AddChild(verts);
   verts.FdPoint.Items.SetLength(0);
   verts.FdPoint.Items.AppendDynArray(obj.Verts);

   texcoords := TNodeTextureCoordinate2.Create('', WWWBasePath);
   result.AddChild(texcoords);
   texcoords.FdPoint.Items.SetLength(0);
   texcoords.FdPoint.Items.AppendDynArray(obj.TexCoords);

   Normals := TNodeNormal.Create('', WWWBasePath);
   Result.AddChild(Normals);
   Normals.FdVector.Items.SetLength(0);
   Normals.FdVector.Items.AppendDynArray(Obj.Normals);

   i := 0;
   while i < obj.Faces.Count do
   begin
    FacesWithTexCoords := Obj.Faces.Items[i].HasTexCoords;
    FacesWithNormals := Obj.Faces.Items[i].HasNormals;
    FacesWithMaterial := Obj.Faces.Items[i].Material;

    FacesSeparator := TNodeSeparator.Create('', WWWBasePath);
    Result.AddChild(FacesSeparator);

    if FacesWithMaterial <> nil then
    begin
      { We use material from the MaterialsSwitch.
        We find it by name, using FindNodeByName, we're sure that we will
        find this material --- since we added all materials to
        MaterialsSwitch. }
      FacesSeparator.AddChild(MaterialsSwitch.FindNodeByName(TVRMLNode,
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
      FacesSeparator.AddChild(texture);
      Texture.FdFilename.Value := OBJModelTextureFilename;
    end;
    *)

    faces := TNodeIndexedFaceSet_1.Create('',WWWBasePath);
    FacesSeparator.AddChild(faces);
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

function Load3dsAsVRML(const filename: string): TVRMLNode;
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
   node.AddChild(camSwitch);
   camSwitch.FdWhichChild.Value := 0;

   for i := 0 to scene.Cameras.Count-1 do
   begin
    camera := MakeVRMLCameraNode(1, WWWBasePath,
      scene.Cameras[i].CamPos,
      scene.Cameras[i].CamDir,
      scene.Cameras[i].CamUp,
      scene.Cameras[i].CamUp { GravityUp equals CamUp });
    camera.NodeName := Camera3dsNameToVRMLName(scene.Cameras[i].Name);
    camSwitch.AddChild(camera);

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
   node.AddChild(lightGroup);

   for i := 0 to Scene.Lights.Count-1 do
   begin
    light := TNodePointLight_1.Create(Light3dsNameToVRMLName(
      Scene.Lights[i].Name), WWWBasePath);
    lightGroup.AddChild(light);

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
  result := TNodeGroup_1.Create(FileNameToVRMLName(filename), WWWBasePath);
  try
   Add3dsCameras(obj3ds, result);
   Add3dsLights(obj3ds, result);

   { konstruuj liste materiali jako dzieci materialSwitch }
   materialSwitch := TNodeSwitch_1.Create('Materials', WWWBasePath);
   result.AddChild(materialSwitch);

   for i := 0 to obj3ds.Materials.Count-1 do
   begin
    materialGroup := TNodeGroup_1.Create(Mat3dsNameToVRMLName(obj3ds.Materials[i].Name), WWWBasePath);
    materialSwitch.AddChild(materialGroup);

    { dodaj Material node }
    tmp := TNodeMaterial_1.Create('',WWWBasePath);
    materialGroup.AddChild(tmp);
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
     tmp := TNodeTexture2.Create('',WWWBasePath);
     materialGroup.AddChild(tmp);
     TNodeTexture2(tmp).FdFilename.Value := obj3ds.Materials[i].TextureMap1.MapFilename;

     tmp := TNodeTexture2Transform.Create('',WWWBasePath);
     materialGroup.AddChild(tmp);
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
    result.AddChild(trimeshGroup);

    { zapisz Coordinate3 }
    trimeshCoords := TNodeCoordinate3.Create('',WWWBasePath);
    trimeshGroup.AddChild(trimeshCoords);
    trimeshCoords.FdPoint.Items.SetLength(trimesh3ds.VertsCount);
    for j := 0 to trimesh3ds.VertsCount-1 do
     trimeshCoords.FdPoint.Items.Items[j] := trimesh3ds.Verts^[j].Pos;

    { zapisz TextureCoordinate2 jesli je mamy }
    if trimesh3ds.HasTexCoords then
    begin
     trimeshTexCoords := TNodeTextureCoordinate2.Create('',WWWBasePath);
     trimeshGroup.AddChild(trimeshTexCoords);
     trimeshTexCoords.FdPoint.Items.SetLength(trimesh3ds.VertsCount);
     for j := 0 to trimesh3ds.VertsCount-1 do
      trimeshTexCoords.FdPoint.Items.Items[j] := trimesh3ds.Verts^[j].TexCoord;
    end;

    { zapisz faces }
    j := 0;
    while j < trimesh3ds.FacesCount do
    begin
     FaceMaterialNum := trimesh3ds.Faces^[j].FaceMaterialIndex;
     facesSep := TNodeSeparator.Create('',WWWBasePath);
     trimeshGroup.AddChild(facesSep);

     { uzyj materialu }
     if FaceMaterialNum >= 0 then
      facesSep.AddChild(materialSwitch.Children[FaceMaterialNum]);
      {else uzyje defaultowego materiala. I o to chodzi.}

     { wylacz teksture jesli material ma teksture ale my nie mamy texCoords }
     if (FaceMaterialNum >= 0) and
        (obj3ds.Materials[FaceMaterialNum].TextureMap1.Exists) and
        (not trimesh3ds.HasTexCoords) then
      facesSep.AddChild(TNodeTexture2.Create('Turn_Texture_Off',WWWBasePath));

     { zapisz faces o tym samym FaceMaterialNum.
       Mamy przynajmniej jedno takie face. }

     ThisMaterialFacesCount := SameMaterialFacesCount(trimesh3ds.Faces,
       trimesh3ds.FacesCount, j);

     indexedFacesNode := TNodeIndexedFaceSet_1.Create('', WWWBasePath);
     facesSep.AddChild(indexedFacesNode);
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

function LoadMD3FrameAsVRML(Md3: TObject3dMD3; FrameNumber: Cardinal;
  const WWWBasePath: string): TVRMLNode;

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
    Result.AddChild(MakeTextureCoordinates(Surface.TextureCoords));
    Result.AddChild(MakeCoordinates(Surface.Vertexes, Surface.VertexesInFrameCount));
    Result.AddChild(MakeIndexes(Surface.Triangles));
  end;

var
  I: Integer;
  Texture: TNodeTexture2;
begin
  Result := TNodeGroup_1.Create(
    ToVRMLName(Md3.Name
      { Although adding here FrameNumber is not a bad idea, but VRMLGLAnimation
        requires for now that sequence of VRML models have the same node names }
      { + '_Frame' + IntToStr(FrameNumber) }), WWWBasePath);

  { MD3 files have no camera. I add camera here, just to force GravityUp
    to be in +Z, since this is the convention used in all MD3 file that
    I saw (so I guess that Quake3 engine generally uses this convention). }
  Result.AddChild(MakeVRMLCameraNode(1, WWWBasePath,
    Vector3Single(0, 0, 0),
    Vector3Single(1, 0, 0),
    Vector3Single(0, 0, 1),
    Vector3Single(0, 0, 1)));

  if Md3.TextureFileName <> '' then
  begin
    Texture := TNodeTexture2.Create('',WWWBasePath);
    Result.AddChild(Texture);
    Texture.FdFilename.Value := Md3.TextureFileName;
  end;

  for I := 0 to Md3.Surfaces.Count - 1 do
    Result.AddChild(MakeSeparator(Md3.Surfaces[I]));
end;

function LoadMD3AsVRML(const FileName: string): TVRMLNode;
var
  Md3: TObject3dMD3;
  WWWBasePath: string;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(FileName));
  Md3 := TObject3dMD3.Create(FileName);
  try
    Result := LoadMD3FrameAsVRML(Md3, 0, WWWBasePath);
  finally FreeAndNil(Md3) end;
end;

procedure LoadMD3AsVRMLSequence(
  const FileName: string;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  var Optimization: TGLRendererOptimization;
  out EqualityEpsilon: Single;
  out TimeLoop, TimeBackwards: boolean);
var
  Md3: TObject3dMD3;
  WWWBasePath: string;
  I: Integer;
begin
  WWWBasePath := ExtractFilePath(ExpandFilename(FileName));
  Md3 := TObject3dMD3.Create(FileName);
  try
    { handle each MD3 frame }
    for I := 0 to Md3.FramesCount - 1 do
    begin
      RootNodes.Add(LoadMD3FrameAsVRML(Md3, I, WWWBasePath));
      Times.AppendItem(I / 30);
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

function LoadColladaAsVRML(const FileName: string): TVRMLNode;
var
  WWWBasePath: string;

  { List of VRML Material nodes storing Collada effects.
    They are kept for handling instance_effect in <material> node, which will
    copy items from this list into Materials list. Materials list will keep
    names of the materials, forgetting about names of effects.
    Many materials may refer to a single effect, that's why for Materials
    list we will copy effects (not just make a reference to them). }
  Effects: TVRMLNodesList;

  { Another list of VRML Material nodes, this time storing Collada materials. }
  Materials: TVRMLNodesList;

  { List of VRML IndexedFaceSet nodes (in general, these are nodes suitable
    for Shape.geometry field) representing <geometry> Collada elements. }
  Geometries: TVRMLNodesList;

  ResultModel: TNodeGroup_2 absolute Result;

  Version14: boolean; //< Collada version >= 1.4.x

  { Read elements of type "common_color_or_texture_type" in Collada >= 1.4.x. }
  function ReadColorOrTexture(Element: TDOMElement): TVector3Single;
  var
    ColorElement: TDOMElement;
  begin
    ColorElement := DOMGetChildElement(Element, 'color', true);
    { I simply drop 4th color component, I don't know what's the use of this
      (alpha is exposed by effect/materials parameter transparency, so color
      alpha is supposed to mean something else ?). }
    Result := Vector3SingleCut(
      Vector4SingleFromStr(DOMGetTextData(ColorElement)));
  end;

  { Read elements of type "common_float_or_param_type" in Collada >= 1.4.x. }
  function ReadFloatOrParam(Element: TDOMElement): Float;
  var
    FloatElement: TDOMElement;
  begin
    FloatElement := DOMGetChildElement(Element, 'float', true);
    Result := StrToFloat(DOMGetTextData(FloatElement));
  end;

  { Read <effect>. Only for Collada >= 1.4.x. }
  procedure ReadEffect(EffectElement: TDOMElement);
  var
    Effect: TNodeMaterial_2;
    Id: string;
    ProfileElement, TechniqueElement, PhongElement: TDOMElement;
    Children: TDOMNodeList;
    ChildNode: TDOMNode;
    ChildElement: TDOMElement;
    I: Integer;
  begin
    if not DOMGetAttribute(EffectElement, 'id', Id) then
      Id := '';

    Effect := TNodeMaterial_2.Create(Id, WWWBasePath);
    Effects.Add(Effect);

    ProfileElement := DOMGetChildElement(EffectElement, 'profile_COMMON', false);
    if ProfileElement <> nil then
    begin
       TechniqueElement := DOMGetChildElement(ProfileElement, 'technique', false);
       if TechniqueElement <> nil then
       begin
         PhongElement := DOMGetChildElement(TechniqueElement, 'phong', false);
         if PhongElement <> nil then
         begin
           Children := PhongElement.ChildNodes;
           try
             for I := 0 to Children.Count - 1 do
             begin
               ChildNode := Children.Item[I];
               if ChildNode.NodeType = ELEMENT_NODE then
               begin
                 ChildElement := ChildNode as TDOMElement;

                 if ChildElement.TagName = 'emission' then
                   Effect.FdEmissiveColor.Value :=
                     ReadColorOrTexture(ChildElement) else

                 if ChildElement.TagName = 'ambient' then
                   Effect.FdAmbientIntensity.Value := VectorAverage(
                     ReadColorOrTexture(ChildElement)) else

                 if ChildElement.TagName = 'diffuse' then
                   Effect.FdDiffuseColor.Value :=
                     ReadColorOrTexture(ChildElement) else

                 if ChildElement.TagName = 'specular' then
                   Effect.FdSpecularColor.Value :=
                     ReadColorOrTexture(ChildElement) else

                 if ChildElement.TagName = 'shininess' then
                   Effect.FdShininess.Value :=
                     ReadFloatOrParam(ChildElement) / 128.0 else

                 if ChildElement.TagName = 'reflective' then
                   {Effect.FdMirrorColor.Value := }
                     ReadColorOrTexture(ChildElement) else

                 if ChildElement.TagName = 'reflectivity' then
                   Effect.FdMirror.Value :=
                     ReadFloatOrParam(ChildElement) else

                 if ChildElement.TagName = 'transparent' then
                   {Effect.FdTransparencyColor.Value := }
                     ReadColorOrTexture(ChildElement) else

                 if ChildElement.TagName = 'transparency' then
                   Effect.FdTransparency.Value :=
                     ReadFloatOrParam(ChildElement) else

                 if ChildElement.TagName = 'index_of_refraction' then
                   {Effect.FdIndexOfRefraction.Value := }
                     ReadFloatOrParam(ChildElement);
               end;
             end;
           finally Children.Release; end
         end;
       end;
    end;
  end;

  { Read <library_effects>. Only for Collada >= 1.4.x. }
  procedure ReadLibraryEffects(LibraryElement: TDOMElement);
  var
    Children: TDOMNodeList;
    ChildNode: TDOMNode;
    ChildElement: TDOMElement;
    I: Integer;
    LibraryId: string;
  begin
    if not DOMGetAttribute(LibraryElement, 'id', LibraryId) then
      LibraryId := '';

    Children := LibraryElement.ChildNodes;
    try
      for I := 0 to Children.Count - 1 do
      begin
        ChildNode := Children.Item[I];
        if ChildNode.NodeType = ELEMENT_NODE then
        begin
          ChildElement := ChildNode as TDOMElement;
          if ChildElement.TagName = 'effect' then
            ReadEffect(ChildElement);
            { other ChildElement.TagName not supported for now }
        end;
      end;
    finally Children.Release; end
  end;

  { Read <material>. }
  procedure ReadMaterial(MatElement: TDOMElement);

    function ReadParamAsVector3(Element: TDOMElement): TVector3Single;
    var
      AType: string;
    begin
      if not DOMGetAttribute(Element, 'type', AType) then
      begin
        DataNonFatalError('<param> has no type attribute');
        Result := ZeroVector3Single;
      end else
      if AType <> 'float3' then
      begin
        DataNonFatalError('Expected <param> with type "float3"');
        Result := ZeroVector3Single;
      end else
        Result := Vector3SingleFromStr(DOMGetTextData(Element));
    end;

    function ReadParamAsFloat(Element: TDOMElement): Float;
    var
      AType: string;
    begin
      if not DOMGetAttribute(Element, 'type', AType) then
      begin
        DataNonFatalError('<param> has no type attribute');
        Result := 0;
      end else
      if AType <> 'float' then
      begin
        DataNonFatalError('Expected <param> with type "float"');
        Result := 0;
      end else
        Result := StrToFloat(DOMGetTextData(Element));
    end;

  var
    MatId: string;

    { For Collada < 1.4.x }
    procedure TryCollada13;
    var
      ShaderElement, TechniqueElement, PassElement, ProgramElement: TDOMElement;
      Children: TDOMNodeList;
      ChildNode: TDOMNode;
      ChildElement: TDOMElement;
      ParamName: string;
      I: Integer;
      Mat: TNodeMaterial_2;
    begin
      Mat := TNodeMaterial_2.Create(MatId, WWWBasePath);
      Materials.Add(Mat);

      ShaderElement := DOMGetChildElement(MatElement, 'shader', false);
      if ShaderElement <> nil then
      begin
         TechniqueElement := DOMGetChildElement(ShaderElement, 'technique', false);
         if TechniqueElement <> nil then
         begin
           PassElement := DOMGetChildElement(TechniqueElement, 'pass', false);
           if PassElement <> nil then
           begin
             ProgramElement := DOMGetChildElement(PassElement, 'program', false);
             if ProgramElement <> nil then
             begin
               Children := ProgramElement.ChildNodes;
               try
                 for I := 0 to Children.Count - 1 do
                 begin
                   ChildNode := Children.Item[I];
                   if ChildNode.NodeType = ELEMENT_NODE then
                   begin
                     ChildElement := ChildNode as TDOMElement;
                     if ChildElement.TagName = 'param' then
                     begin
                       if DOMGetAttribute(ChildElement, 'name', ParamName) then
                       begin
                         if ParamName = 'EMISSION' then
                           Mat.FdEmissiveColor.Value :=
                             ReadParamAsVector3(ChildElement) else

                         if ParamName = 'AMBIENT' then
                           Mat.FdAmbientIntensity.Value := VectorAverage(
                             ReadParamAsVector3(ChildElement)) else

                         if ParamName = 'DIFFUSE' then
                           Mat.FdDiffuseColor.Value :=
                             ReadParamAsVector3(ChildElement) else

                         if ParamName = 'SPECULAR' then
                           Mat.FdSpecularColor.Value :=
                             ReadParamAsVector3(ChildElement) else

                         if ParamName = 'SHININESS' then
                           Mat.FdShininess.Value :=
                             ReadParamAsFloat(ChildElement) / 128.0 else

                         if ParamName = 'REFLECTIVE' then
                           {Mat.FdMirrorColor.Value := }
                             ReadParamAsVector3(ChildElement) else

                         if ParamName = 'REFLECTIVITY' then
                           Mat.FdMirror.Value :=
                             ReadParamAsFloat(ChildElement) else

                         (*
                         Blender Collada 1.3.1 sets type to "float" here
                         (although content inicates "float3", consistently with
                         what is in Collada 1.4.1 spec.)
                         I don't handle this anyway, so I just ignore it for now,
                         until I'll read Collada 1.3.1 spec and report this to
                         Blender.

                         if ParamName = 'TRANSPARENT' then
                           {Mat.FdTransparencyColor.Value := }
                             ReadParamAsVector3(ChildElement) else
                         *)

                         if ParamName = 'TRANSPARENCY' then
                           Mat.FdTransparency.Value :=
                             ReadParamAsFloat(ChildElement);

                         { other ParamName not handled }
                       end;
                     end;
                   end;
                 end;
               finally Children.Release; end
             end;
           end;
         end;
      end;
    end;

    { For Collada >= 1.4.x }
    procedure TryCollada14;
    var
      InstanceEffect: TDOMElement;
      EffectId: string;
      Mat: TNodeMaterial_2;
      EffectIndex: Integer;
    begin
      if MatId = '' then Exit;

      InstanceEffect := DOMGetChildElement(MatElement, 'instance_effect', false);
      if InstanceEffect <> nil then
      begin
        if DOMGetAttribute(InstanceEffect, 'url', EffectId) and
           SCharIs(EffectId, 1, '#') then
        begin
          Delete(EffectId, 1, 1); { delete initial '#' char }
          { tests: Writeln('instantiating effect ', EffectId, ' as material ', MatId); }

          EffectIndex := Effects.FindNodeName(EffectId);
          if EffectIndex <> -1 then
          begin
            Mat := VRMLNodeDeepCopy(Effects[EffectIndex]) as TNodeMaterial_2;
            Mat.NodeName := MatId;
            Materials.Add(Mat);
          end else
            DataNonFatalError(Format('Collada material "%s" references ' +
              'non-existing effect "%s"', [MatId, EffectId]));
        end;
      end;
    end;

  begin
    if not DOMGetAttribute(MatElement, 'id', MatId) then
      MatId := '';

    if Version14 then
      TryCollada14 else
      TryCollada13;
  end;

  { Read <library_materials> (Collada >= 1.4.x) or
    <library type="MATERIAL"> (Collada < 1.4.x). }
  procedure ReadLibraryMaterials(LibraryElement: TDOMElement);
  var
    Children: TDOMNodeList;
    ChildNode: TDOMNode;
    ChildElement: TDOMElement;
    I: Integer;
    LibraryId: string;
  begin
    if not DOMGetAttribute(LibraryElement, 'id', LibraryId) then
      LibraryId := '';

    Children := LibraryElement.ChildNodes;
    try
      for I := 0 to Children.Count - 1 do
      begin
        ChildNode := Children.Item[I];
        if ChildNode.NodeType = ELEMENT_NODE then
        begin
          ChildElement := ChildNode as TDOMElement;
          if ChildElement.TagName = 'material' then
            ReadMaterial(ChildElement);
            { other ChildElement.TagName not supported for now }
        end;
      end;
    finally Children.Release; end
  end;

  { Read <geometry> }
  procedure ReadGeometry(GeometryElement: TDOMElement);
  var
    GeometryId: string;

    { Text is the name of the source, Object is the TDynVector3SingleArray
      instance containing this source's data. }
    SourcesList: TStringList;

    { Read <source> within <mesh>.

      This is quite limited compared to what Collada offers, actually
      we understand only float_array data accessed by accessor with
      three parameters (although any accessor.stride >= 3).

      In other words, <source> simply defines a named array of TVector3Single
      for us. }
    procedure ReadSource(SourceElement: TDOMElement);
    var
      FloatArray, Technique, Accessor: TDOMElement;
      SeekPos, FloatArrayCount, I, AccessorCount, AccessorStride,
        AccessorOffset, MinCount: Integer;
      Floats: TDynFloatArray;
      SourceId, FloatArrayContents, Token, AccessorSource: string;
      Source: TDynVector3SingleArray;
    begin
      if not DOMGetAttribute(SourceElement, 'id', SourceId) then
        SourceId := '';

      FloatArray := DOMGetChildElement(SourceElement, 'float_array', false);
      if FloatArray <> nil then
      begin
        if not DOMGetIntegerAttribute(FloatArray, 'count', FloatArrayCount) then
        begin
          FloatArrayCount := 0;
          DataNonFatalError('Collada <float_array> without a count attribute');
        end;

        Floats := TDynFloatArray.Create(FloatArrayCount);
        try
          FloatArrayContents := DOMGetTextData(FloatArray);

          SeekPos := 1;
          for I := 0 to FloatArrayCount - 1 do
          begin
            Token := NextToken(FloatArrayContents, SeekPos, WhiteSpaces);
            if Token = '' then
            begin
              DataNonFatalError('Collada: actual number of tokens in <float_array>' +
                ' less than declated in the count attribute');
              Break;
            end;
            Floats.Items[I] := StrToFloat(Token);
          end;

          Technique := DOMGetChildElement(SourceElement, 'technique_common', false);
          if Technique = nil then
          begin
            Technique := DOMGetChildElement(SourceElement, 'technique', false);
            { TODO: actually, I should search for technique with profile="COMMON"
              in this case, right ? }
          end;

          if Technique <> nil then
          begin
            Accessor := DOMGetChildElement(Technique, 'accessor', false);

            { read <accessor> attributes }

            if not DOMGetIntegerAttribute(Accessor, 'count', AccessorCount) then
            begin
              DataNonFatalError('Collada: <accessor> has no count attribute');
              AccessorCount := 0;
            end;

            if not DOMGetIntegerAttribute(Accessor, 'stride', AccessorStride) then
              { default, according to Collada spec }
              AccessorStride := 1;

            if not DOMGetIntegerAttribute(Accessor, 'offset', AccessorOffset) then
              { default, according to Collada spec }
              AccessorOffset := 0;

            if not DOMGetAttribute(Accessor, 'source', AccessorSource) then
            begin
              DataNonFatalError('Collada: <accessor> has no source attribute');
              AccessorSource := '';
            end;
            { TODO: we ignore AccessorSource, just assume that it refers to
              float_array within this <source> }

            if AccessorStride >= 3 then
            begin
              { Max index accessed is
                  AccessorOffset + AccessorStride * (AccessorCount - 1) + 2.
                Minimum count is +1 to this.
                Check do we have enough floats. }
              MinCount := AccessorOffset + AccessorStride * (AccessorCount - 1) + 3;
              if Floats.Count < MinCount then
              begin
                DataNonFatalError(Format('Collada: <accessor> count requires at least %d float ' +
                  'values (offset %d + stride %d * (count %d - 1) + 3) in <float_array>, ' +
                  'but only %d are avilable', [MinCount,
                    AccessorOffset, AccessorStride, AccessorCount, Floats.Count]));
                { force AccessorCount smaller }
                AccessorCount := (Floats.Count - AccessorOffset) div AccessorStride;
              end;

              Source := TDynVector3SingleArray.Create(AccessorCount);
              for I := 0 to AccessorCount - 1 do
              begin
                Source.Items[I][0] := Floats.Items[AccessorOffset + AccessorStride * I    ];
                Source.Items[I][1] := Floats.Items[AccessorOffset + AccessorStride * I + 1];
                Source.Items[I][2] := Floats.Items[AccessorOffset + AccessorStride * I + 2];
              end;
              { tests: Writeln('added source ', SourceId, ' with ', AccessorCount, ' items'); }
              SourcesList.AddObject(SourceId, Source);
            end;
          end;
        finally FreeAndNil(Floats); end;
      end;
    end;

  var
    { This is just a reference to one of the objects on SourcesList --- the one
      referenced by <vertices> element. }
    Vertices: TDynVector3SingleArray;
    VerticesId: string;

    { Read <vertices> within <mesh> }
    procedure ReadVertices(VerticesElement: TDOMElement);
    var
      Children: TDOMNodeList;
      ChildNode: TDOMNode;
      ChildElement: TDOMElement;
      I: Integer;
      InputSemantic, InputSource: string;
      InputSourceIndex: Integer;
    begin
      if not DOMGetAttribute(VerticesElement, 'id', VerticesId) then
        VerticesId := '';

      Children := VerticesElement.ChildNodes;
      try
        for I := 0 to Children.Count - 1 do
        begin
          ChildNode := Children.Item[I];
          if ChildNode.NodeType = ELEMENT_NODE then
          begin
            ChildElement := ChildNode as TDOMElement;
            if (ChildElement.TagName = 'input') and
               DOMGetAttribute(ChildElement, 'semantic', InputSemantic) and
               (InputSemantic = 'POSITION') and
               DOMGetAttribute(ChildElement, 'source', InputSource) and
               SCharIs(InputSource, 1, '#') then
            begin
              Delete(InputSource, 1, 1); { delete leading '#' char }
              InputSourceIndex := SourcesList.IndexOf(InputSource);
              if InputSourceIndex <> -1 then
              begin
                Vertices := SourcesList.Objects[InputSourceIndex] as
                  TDynVector3SingleArray;
                Exit;
              end else
              begin
                DataNonFatalError(Format('Collada: source attribute ' +
                  '(of <input> element within <vertices>) ' +
                  'references non-existing source "%s"', [InputSource]));
              end;
            end;
          end;
        end;
      finally Children.Release; end;

      DataNonFatalError('Collada: <vertices> element has no <input> child' +
        ' with semantic="POSITION" and some source attribute');
    end;

    { Read <polygons> within <mesh> }
    procedure ReadPolygons(PolygonsElement: TDOMElement);
    var
      IndexedFaceSet: TNodeIndexedFaceSet_2;
      VerticesOffset: Integer;
      InputsCount: Integer;

      procedure AddPolygon(const Indexes: string);
      var
        SeekPos, Index, CurrentInput: Integer;
        Token: string;
      begin
        CurrentInput := 0;
        SeekPos := 1;

        repeat
          Token := NextToken(Indexes, SeekPos, WhiteSpaces);
          if Token = '' then Break;
          Index := StrToInt(Token);

          { for now, we just ignore indexes to other inputs }
          if CurrentInput = VerticesOffset then
            IndexedFaceSet.FdCoordIndex.Items.AppendItem(Index);

          Inc(CurrentInput);
          if CurrentInput = InputsCount then CurrentInput := 0;
        until false;

        IndexedFaceSet.FdCoordIndex.Items.AppendItem(-1);
      end;

    var
      Children: TDOMNodeList;
      ChildNode: TDOMNode;
      ChildElement: TDOMElement;
      I: Integer;
      Coord: TNodeCoordinate;
      PolygonsCount: Integer;
      InputSemantic, InputSource: string;
    begin
      if not DOMGetIntegerAttribute(PolygonsElement, 'count', PolygonsCount) then
        PolygonsCount := 0;

      VerticesOffset := 0;

      IndexedFaceSet := TNodeIndexedFaceSet_2.Create(GeometryId, WWWBasePath);
      Geometries.Add(IndexedFaceSet);

      IndexedFaceSet.FdCoordIndex.Items.Count := 0;
      IndexedFaceSet.FdSolid.Value := false;

      Coord := TNodeCoordinate.Create(VerticesId, WWWBasePath);
      IndexedFaceSet.FdCoord.Value := Coord;
      Coord.FdPoint.Items.Assign(Vertices);

      InputsCount := 0;

      Children := PolygonsElement.ChildNodes;
      try
        for I := 0 to Children.Count - 1 do
        begin
          ChildNode := Children.Item[I];
          if ChildNode.NodeType = ELEMENT_NODE then
          begin
            ChildElement := ChildNode as TDOMElement;
            if ChildElement.TagName = 'input' then
            begin
              { we must count all inputs, since parsing <p> elements depends
                on InputsCount }
              Inc(InputsCount);
              if DOMGetAttribute(ChildElement, 'semantic', InputSemantic) and
                 (InputSemantic = 'VERTEX') then
              begin
                if not (DOMGetAttribute(ChildElement, 'source', InputSource) and
                        (InputSource = '#' + VerticesId))  then
                  DataNonFatalError('Collada: <input> with semantic="VERTEX" ' +
                    '(of <polygons> element within <mesh>) does not reference ' +
                    '<vertices> element within the same <mesh>');

                { Collada requires offset in this case.
                  For us, if there's no offset, just leave VerticesOffset as it was. }
                DOMGetIntegerAttribute(ChildElement, 'offset', VerticesOffset);
              end;
            end else
            if ChildElement.TagName = 'p' then
              AddPolygon(DOMGetTextData(ChildElement));
          end;
        end;
      finally Children.Release; end;
    end;

  var
    Children: TDOMNodeList;
    ChildNode: TDOMNode;
    ChildElement, Mesh: TDOMElement;
    I: Integer;
  begin
    if not DOMGetAttribute(GeometryElement, 'id', GeometryId) then
      GeometryId := '';

    Mesh := DOMGetChildElement(GeometryElement, 'mesh', false);
    if Mesh <> nil then
    begin
      SourcesList := TStringList.Create;
      try
        Children := Mesh.ChildNodes;
        try
          for I := 0 to Children.Count - 1 do
          begin
            ChildNode := Children.Item[I];
            if ChildNode.NodeType = ELEMENT_NODE then
            begin
              ChildElement := ChildNode as TDOMElement;
              if ChildElement.TagName = 'source' then
                ReadSource(ChildElement) else
              if ChildElement.TagName = 'vertices' then
                ReadVertices(ChildElement) else
              if ChildElement.TagName = 'polygons' then
                ReadPolygons(ChildElement);
                { other ChildElement.TagName not supported for now }
            end;
          end;
        finally Children.Release; end
      finally StringList_FreeWithContentsAndNil(SourcesList); end;
    end;
  end;

  { Read <library_geometries> (Collada >= 1.4.x) or
    <library type="GEOMETRY"> (Collada < 1.4.x). }
  procedure ReadLibraryGeometries(LibraryElement: TDOMElement);
  var
    Children: TDOMNodeList;
    ChildNode: TDOMNode;
    ChildElement: TDOMElement;
    I: Integer;
    LibraryId: string;
  begin
    if not DOMGetAttribute(LibraryElement, 'id', LibraryId) then
      LibraryId := '';

    Children := LibraryElement.ChildNodes;
    try
      for I := 0 to Children.Count - 1 do
      begin
        ChildNode := Children.Item[I];
        if ChildNode.NodeType = ELEMENT_NODE then
        begin
          ChildElement := ChildNode as TDOMElement;
          if ChildElement.TagName = 'geometry' then
            ReadGeometry(ChildElement);
            { other ChildElement.TagName not supported for now }
        end;
      end;
    finally Children.Release; end
  end;

  { Read <library> element.
    Only for Collada < 1.4.x (Collada >= 1.4.x has  <library_xxx> elements). }
  procedure ReadLibrary(LibraryElement: TDOMElement);
  var
    LibraryType: string;
  begin
    if DOMGetAttribute(LibraryElement, 'type', LibraryType) then
    begin
      if LibraryType = 'MATERIAL' then
        ReadLibraryMaterials(LibraryElement) else
      if LibraryType = 'GEOMETRY' then
        ReadLibraryGeometries(LibraryElement);
        { other LibraryType not supported for now }
    end;
  end;

  { Read <matrix> element, add appropriate VRML MatrixTransform to ParentGroup node. }
  procedure ReadMatrix(ParentGroup: TNodeX3DGroupingNode; MatrixElement: TDOMElement);
  var
    SeekPos: Integer;
    M: TNodeMatrixTransform;
    Row, Col: Integer;
    Token, Indexes: string;
  begin
    { We have to use VRML 1.0 to express matrix transform. }
    M := TNodeMatrixTransform.Create('', WWWBasePath);
    ParentGroup.FdChildren.AddItem(M);

    Indexes := DOMGetTextData(MatrixElement);

    SeekPos := 1;

    for Row := 0 to 3 do
      for Col := 0 to 3 do
      begin
        Token := NextToken(Indexes, SeekPos, WhiteSpaces);
        if Token = '' then
        begin
          DataNonFatalError('Collada <matrix> has not enough items');
          Break;
        end;
        M.FdMatrix.Matrix[Col, Row] := StrToFloat(Token);
      end;
  end;

  { Read <node> element, add it to ParentGroup. }
  procedure ReadNodeElement(ParentGroup: TNodeX3DGroupingNode;
    NodeElement: TDOMElement);
  var
    Children: TDOMNodeList;
    ChildNode: TDOMNode;
    ChildElement: TDOMElement;
    I: Integer;
    NodeId: string;
    Matrix, Instance: TDOMElement;
    NodeTransform: TNodeTransform_2;
    Shape: TNodeShape;
    GeometryId: string;
    GeometryIndex: Integer;
  begin
    if not DOMGetAttribute(NodeElement, 'id', NodeId) then
      NodeId := '';

    NodeTransform := TNodeTransform_2.Create(NodeId, WWWBasePath);
    ParentGroup.FdChildren.AddItem(NodeTransform);

    { TODO: other transform properties, setting NodeTransform fields. }

    Matrix := DOMGetChildElement(NodeElement, 'matrix', false);
    if Matrix <> nil then
    begin
      ReadMatrix(NodeTransform, Matrix);
    end;

    Instance := DOMGetChildElement(NodeElement, 'instance', false);
    if (Instance <> nil) and
       DOMGetAttribute(Instance, 'url', GeometryId) and
       SCharIs(GeometryId, 1, '#') then
    begin
      Delete(GeometryId, 1, 1);
      GeometryIndex := Geometries.FindNodeName(GeometryId);
      if GeometryIndex = -1 then
      begin
        DataNonFatalError(Format('Collada <node> "%s" instantiates non-existing ' +
          '<geometry> element "%s"', [NodeId, GeometryId]));
      end else
      begin
        Shape := TNodeShape.Create('', WWWBasePath);
        NodeTransform.FdChildren.AddItem(Shape);
        Shape.FdGeometry.Value := Geometries[GeometryIndex];
      end;
    end;

    Children := NodeElement.ChildNodes;
    try
      for I := 0 to Children.Count - 1 do
      begin
        ChildNode := Children.Item[I];
        if ChildNode.NodeType = ELEMENT_NODE then
        begin
          ChildElement := ChildNode as TDOMElement;
          if ChildElement.TagName = 'node' then
            ReadNodeElement(NodeTransform, ChildElement);
        end;
      end;
    finally Children.Release; end
  end;

  { Read <scene> element. }
  procedure ReadSceneElement(SceneElement: TDOMElement);
  var
    Children: TDOMNodeList;
    ChildNode: TDOMNode;
    ChildElement: TDOMElement;
    I: Integer;
    SceneId: string;
    Group: TNodeGroup_2;
  begin
    if not DOMGetAttribute(SceneElement, 'id', SceneId) then
      SceneId := '';

    if not Version14 then
    begin
      Group := TNodeGroup_2.Create(SceneId, WWWBasePath);
      ResultModel.FdChildren.AddItem(Group);

      Children := SceneElement.ChildNodes;
      try
        for I := 0 to Children.Count - 1 do
        begin
          ChildNode := Children.Item[I];
          if ChildNode.NodeType = ELEMENT_NODE then
          begin
            ChildElement := ChildNode as TDOMElement;
            if ChildElement.TagName = 'node' then
              ReadNodeElement(Group, ChildElement);
          end;
        end;
      finally Children.Release; end
    end;
  end;

  procedure AddInfo(Element: TNodeGroup_2; const S: string);
  var
    Info: TNodeInfo;
  begin
    Info := TNodeInfo.Create('', WWWBasePath);
    Element.FdChildren.AddItem(Info);
    Info.FdString.Value := S;
  end;

var
  Doc: TXMLDocument;
  Version: string;
  I: Integer;
  DocChildren: TDOMNodeList;
  ChildNode: TDOMNode;
  ChildElement: TDOMElement;
begin
  Effects := nil;
  Materials := nil;
  Geometries := nil;

  ReadXMLFile(Doc, FileName);
  try
    Check(Doc.DocumentElement.TagName = 'COLLADA',
      'Root node of Collada file must be <COLLADA>');

    if not DOMGetAttribute(Doc.DocumentElement, 'version', Version) then
    begin
      Version := '';
      Version14 := false;
      DataNonFatalError('<COLLADA> element misses "version" attribute');
    end else
    begin
      { TODO: uhm, terrible hack... I should move my lazy ass and tokenize
        Version properly. }
      Version14 := IsPrefix('1.4.', Version);
    end;

    if DOMGetAttribute(Doc.DocumentElement, 'base', WWWBasePath) then
    begin
      { COLLADA.base is exactly for the same purpose as WWWBasePath.
        Use it (making sure it's absolute path). }
      WWWBasePath := ExpandFileName(WWWBasePath);
    end else
      WWWBasePath := ExtractFilePath(ExpandFilename(FileName));

    Effects := TVRMLNodesList.Create;
    Materials := TVRMLNodesList.Create;
    Geometries := TVRMLNodesList.Create;

    Result := TNodeGroup_2.Create('', WWWBasePath);
    try

      DocChildren := Doc.DocumentElement.ChildNodes;
      try
        for I := 0 to DocChildren.Count - 1 do
        begin
          ChildNode := DocChildren.Item[I];
          if ChildNode.NodeType = ELEMENT_NODE then
          begin
            ChildElement := ChildNode as TDOMElement;
            if ChildElement.TagName = 'library' then { only Collada < 1.4.x }
              ReadLibrary(ChildElement) else
            if ChildElement.TagName = 'library_materials' then { only Collada >= 1.4.x }
              ReadLibraryMaterials(ChildElement) else
            if ChildElement.TagName = 'library_effects' then { only Collada >= 1.4.x }
              ReadLibraryEffects(ChildElement) else
            if ChildElement.TagName = 'library_geometries' then { only Collada >= 1.4.x }
              ReadLibraryGeometries(ChildElement) else
            if ChildElement.TagName = 'scene' then
              ReadSceneElement(ChildElement);
              { other ChildElement.TagName not supported for now }
          end;
        end;
      finally DocChildren.Release; end;

      AddInfo(Result as TNodeGroup_2,
        'Converted from Collada version "' + Version + '" by ' +
        'Kambi VRML game engine [http://vrmlengine.sourceforge.net/]');
    except FreeAndNil(Result); raise; end;
  finally
    FreeAndNil(Doc);
    FreeWithContentsAndNil(Effects);
    { TODO: these may be refereced somewhere, only without parents should be freed. }
    FreeWithContentsAndNil(Materials);
    VRMLNodesList_FreeWithNonParentedContentsAndNil(Geometries);
  end;
end;

function LoadAsVRML(const filename: string; AllowStdIn: boolean): TVRMLNode;
const
  Extensions: array [0..8] of string =
  ('.geo', '.3ds', '.obj', '.iv', '.wrl', '.gz', '.wrz', '.md3', '.dae');
var
  Ext: string;
begin
  if AllowStdIn and (FileName = '-') then
    result := ParseVRMLFile('-', true) else
  begin
    Ext := ExtractFileExt(filename);
    case ArrayPosText(Ext, Extensions) of
      0: result := LoadGEOAsVRML(filename);
      1: result := Load3dsAsVRML(filename);
      2: result := LoadOBJAsVRML(filename);
      3..6: result := ParseVRMLFile(filename, false);
      7: Result := LoadMD3AsVRML(FileName);
      8: Result := LoadColladaAsVRML(FileName);
      else raise Exception.CreateFmt(
        'Unrecognized file extension "%s" for 3D model file "%s"',
        [Ext, FileName]);
    end;
  end;
end;

procedure LoadAsVRMLSequence(const FileName: string; AllowStdIn: boolean;
  RootNodes: TVRMLNodesList;
  Times: TDynSingleArray;
  out ScenesPerTime: Cardinal;
  var Optimization: TGLRendererOptimization;
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
        ScenesPerTime, Optimization, EqualityEpsilon,
        TimeLoop, TimeBackwards);

      Assert(ModelFileNames.Length = Times.Length);
      Assert(ModelFileNames.Length >= 1);

      { Now use ModelFileNames to load RootNodes }
      RootNodes.Count := ModelFileNames.Count;
      for I := 0 to ModelFileNames.High do
      try
        RootNodes[I] := LoadAsVRML(ModelFileNames[I]);
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
    Times.AppendItem(0); { One time value }
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
    LoadMD3AsVRMLSequence(FileName, RootNodes, Times, ScenesPerTime,
      Optimization, EqualityEpsilon, TimeLoop, TimeBackwards) else
    LoadSingle(LoadAsVRML(FileName, AllowStdIn));
end;

end.
