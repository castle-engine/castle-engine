{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Converting 3d models to VRML.
  Reads models from file to @link(TVRMLNode) object.)

  Note: remember that you may want to embed such @link(TVRMLNode) object
  inside VRML Separator node before inserting it into some existing
  VRML scene. }

unit Object3dAsVRML;

interface

uses VectorMath, SysUtils, VRMLNodes, VRMLFields, Boxes3d;

function LoadGEOAsVRML(const filename: string): TVRMLNode;

(*
  Because OBJ reading code can read texture coords from OBJ file
  but cannot read a texture filename (how is it supposed to
  be written in OBJ file ?!), so before every IndexedFaceSet node
  that has some texture coords @link(LoadOBJAsVRML) inserts node
     @code(Texture2 { filename OBJModelTextureFilename })
  And before every IndexedFaceSet that does not have texture coords
  it inserts @code(Texture2 { }) to turn texture off.

  This way you can put a texture in filename OBJModelTextureFilename
  and it will be correctly used with given OBJ file. *)
var OBJModelTextureFilename: string = 'default_obj_texture.png';

function LoadOBJAsVRML(const filename: string): TVRMLNode;

function Load3dsAsVRML(const filename: string): TVRMLNode;

{ This guesses model format basing on ExtractFileExt(filename),
  then loads model converting it to VRML with appropriate
  LoadXxxAsVRML functions above in this unit or using
  @link(ParseVRMLFile) if this seems to be a VRML file.

  If AllowStdIn and FileName = '-' then it will load a VRML file
  from StdInStream (using GetCurrentDir as WWWBasePath). }
function LoadAsVRML(const filename: string; AllowStdIn: boolean): TVRMLNode;

implementation

uses Object3dGEO, Object3ds, Object3dOBJ, KambiUtils, VRMLCameraUtils;

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
    faces: TNodeIndexedFaceSet;
    i: integer;
    WWWBasePath: string;
begin
 WWWBasePath := ExtractFilePath(ExpandFilename(filename));
 geo := TObject3dGEO.Create(filename);
 try
  result := TNodeGroup.Create(FileNameToVRMLName(filename), WWWBasePath);
  try
   verts := TNodeCoordinate3.Create('',WWWBasePath);
   result.AddChild(verts);
   faces := TNodeIndexedFaceSet.Create('',WWWBasePath);
   result.AddChild(faces);

   verts.FdPoint.Items.SetLength(0);
   verts.FdPoint.Items.AppendArray(geo.Verts.Items^, geo.Verts.Count);

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
    bardzo duze dopuszczalne AllowedCapacityOverflow zeby wszystko bylo
    szybko.
    sorry - powinienes unikac uzywania tego, zrob tu tak jak w Load3dsAsVRML.}
  ALLOWED_INDICES_ARRAYS_OVERFLOWS = 100;

var obj: TObject3dOBJ;
    verts: TNodeCoordinate3;
    faces: TNodeIndexedFaceSet;
    texcoords: TNodeTextureCoordinate2;
    texture: TNodeTexture2;
    i: integer;
    fourIndices: array[0..3]of Longint;
    FacesWithTexCoords: boolean;
    WWWBasePath: string;
begin
 WWWBasePath := ExtractFilePath(ExpandFilename(filename));
 obj := TObject3dOBJ.Create(filename);
 try
  result := TNodeGroup.Create(FileNameToVRMLName(filename), WWWBasePath);
  try
   verts := TNodeCoordinate3.Create('',WWWBasePath);
   result.AddChild(verts);
   verts.FdPoint.Items.SetLength(0);
   verts.FdPoint.Items.AppendDynArray(obj.Verts);

   texcoords := TNodeTextureCoordinate2.Create('',WWWBasePath);
   result.AddChild(texcoords);
   texcoords.FdPoint.Items.SetLength(0);
   texcoords.FdPoint.Items.AppendDynArray(obj.TexCoords);

   i := 0;
   while i < obj.Faces.Count do
   begin
    FacesWithTexCoords := obj.Faces.Items[i].HasTexCoords;

    (* przed kazda grupa IndexedFaceSet dodajemy node Texture2. Jezeli ta grupa
       scian ma texture coords to dajemy jej
         Texture2 { filename "default_obj_texture.png" }
       (co wlaczy defaultowa texture) wpp. dajemy
         Texture2 { }
       (co wylaczy teksturowanie) *)
    texture := TNodeTexture2.Create('',WWWBasePath);
    result.AddChild(texture);
    if FacesWithTexCoords then
     texture.FdFilename.Value := OBJModelTextureFilename;

    faces := TNodeIndexedFaceSet.Create('',WWWBasePath);
    faces.FdCoordIndex.Items.SetLength(0);
    faces.FdCoordIndex.Items.AllowedCapacityOverflow := ALLOWED_INDICES_ARRAYS_OVERFLOWS;
    faces.FdTextureCoordIndex.Items.SetLength(0);
    faces.FdTextureCoordIndex.Items.AllowedCapacityOverflow := ALLOWED_INDICES_ARRAYS_OVERFLOWS;
    result.AddChild(faces);

    {zapisujemy Faces dopoki (FacesWithTexCoords = obj.Faces[i].HasTexCoords).
     Na pewno mamy przed soba przynajmniej jedno takie face. }
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
     Inc(i);
    until (i >= obj.Faces.Count) or 
      (FacesWithTexCoords <> obj.Faces.Items[i].HasTexCoords);

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
  var camSwitch: TNodeSwitch;
      camera: TNodePerspectiveCamera;
      i: Integer;
  begin
   if scene.Cameras.Count = 0 then Exit;

   camSwitch := TNodeSwitch.Create(CamerasSwitchName, WWWBasePath);
   node.AddChild(camSwitch);
   camSwitch.FdWhichChild.Value := 0;

   for i := 0 to scene.Cameras.Count-1 do
   begin
    camera := TNodePerspectiveCamera.Create(Camera3dsNameToVRMLName(
      scene.Cameras[i].Name), WWWBasePath);
    camSwitch.AddChild(camera);

    camera.FdPosition.Value :=  scene.Cameras[i].CamPos;
    camera.FdOrientation.Value :=  CamDirUp2Orient(scene.Cameras[i].CamDir,
      scene.Cameras[i].CamUp);
    {sorry - pozostale pola VRMLa ustalac, wykorzystac pozostale pola kamery 3ds }
   end;
  end;

  procedure Add3dsLights(scene: TScene3ds; node: TVRMLNode);
  var i: Integer;
      light: TNodePointLight;
      lightGroup: TNodeGroup;
  begin
   if Scene.Lights.Count = 0 then Exit;

   lightGroup := TNodeGroup.Create('Lights', WWWBasePath);
   node.AddChild(lightGroup);

   for i := 0 to Scene.Lights.Count-1 do
   begin
    light := TNodePointLight.Create(Light3dsNameToVRMLName(
      Scene.Lights[i].Name), WWWBasePath);
    lightGroup.AddChild(light);

    light.FdOn.Value := Scene.Lights[i].Enabled;
    light.FdLocation.Value := Scene.Lights[i].Pos;
    light.FdColor.Value := Scene.Lights[i].Col;
   end;
  end;

var obj3ds: TScene3ds;
    trimesh3ds: TTrimesh3ds;

    materialSwitch: TNodeSwitch;
    materialGroup, trimeshGroup: TNodeGroup;
    indexedFacesNode: TNodeIndexedFaceSet;
    trimeshCoords: TNodeCoordinate3;
    trimeshTexCoords: TNodeTextureCoordinate2;
    facesSep: TNodeSeparator;

    tmp: TVRMLNode;
    i, j, FaceMaterialNum, ThisMaterialFacesCount, FaceNum: integer;
begin
 WWWBasePath := ExtractFilePath(ExpandFilename(filename));
 obj3ds := TScene3ds.Create(filename);
 try
  result := TNodeGroup.Create(FileNameToVRMLName(filename), WWWBasePath);
  try
   Add3dsCameras(obj3ds, result);
   Add3dsLights(obj3ds, result);

   { konstruuj liste materiali jako dzieci materialSwitch }
   materialSwitch := TNodeSwitch.Create('Materials', WWWBasePath);
   result.AddChild(materialSwitch);

   for i := 0 to obj3ds.Materials.Count-1 do
   begin
    materialGroup := TNodeGroup.Create(Mat3dsNameToVRMLName(obj3ds.Materials[i].Name), WWWBasePath);
    materialSwitch.AddChild(materialGroup);

    { dodaj Material node }
    tmp := TNodeMaterial.Create('',WWWBasePath);
    materialGroup.AddChild(tmp);
    TNodeMaterial(tmp).FdAmbientColor.Items.SetLength(1);
    TNodeMaterial(tmp).FdAmbientColor.Items.Items[0] := Vector3SingleCut(obj3ds.Materials[i].AmbientCol);
    TNodeMaterial(tmp).FdDiffuseColor.Items.SetLength(1);
    TNodeMaterial(tmp).FdDiffuseColor.Items.Items[0] := Vector3SingleCut(obj3ds.Materials[i].DiffuseCol);
    TNodeMaterial(tmp).FdSpecularColor.Items.SetLength(1);
    TNodeMaterial(tmp).FdSpecularColor.Items.Items[0] := Vector3SingleCut(obj3ds.Materials[i].SpecularCol);
    TNodeMaterial(tmp).FdShininess.Items.SetLength(1);
    TNodeMaterial(tmp).FdShininess.Items.Items[0] := obj3ds.Materials[i].Shininess;
    TNodeMaterial(tmp).FdTransparency.Items.SetLength(1);
    TNodeMaterial(tmp).FdTransparency.Items.Items[0] := obj3ds.Materials[i].Transparency;

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

    trimeshGroup := TNodeGroup.Create(Trimesh3dsNameToVRMLName(trimesh3ds.Name), WWWBasePath);
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

     indexedFacesNode := TNodeIndexedFaceSet.Create('', WWWBasePath);
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
      indexedFacesNode.FdTextureCoordIndex.Items.AppendArray(
        indexedFacesNode.FdCoordIndex.Items.Items^,
        indexedFacesNode.FdCoordIndex.Items.Count);
    end;

   end;
  except result.Free; raise end;
 finally obj3ds.Free end;
end;

function LoadAsVRML(const filename: string; AllowStdIn: boolean): TVRMLNode;
const 
  Extensions: array[0..5]of string = 
  ('.geo', '.3ds', '.obj', '.iv', '.wrl', '.gz');
begin
 if AllowStdIn and (FileName = '-') then
  result := ParseVRMLFile('-', true) else
 case ArrayPosText(ExtractFileExt(filename), Extensions) of
  0: result := LoadGEOAsVRML(filename);
  1: result := Load3dsAsVRML(filename);
  2: result := LoadOBJAsVRML(filename);
  3, 4, 5: result := ParseVRMLFile(filename, false);
  else raise Exception.Create('unrecognized file extension for 3d model file : file '''+filename+'''');
 end;
end;

end.
