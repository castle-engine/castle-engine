{
  Copyright 2002-2007 Michalis Kamburelis.

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

{ @abstract(Simple OpenGL renderers and helpers for OBJ, GEO and 3DS models.)

  All of them can be put inside OpenGL display list.

  Note that you will usually do not want to use this unit in your programs,
  since it's much better to convert all OBJ, GEO and 3DS models to VRML
  (e.g. load them using LoadAsVRML) and then render using sophisticated
  and really optimized VRML renderer in @link(VRMLFlatSceneGL).

  This unit just demonstrates that you can, if you really want,
  directly render OBJ or GEO or 3DS models to OpenGL.
  But, at least for now, this has no advantage (well, maybe some simplicity)
  over rendering OBJ, GEO and 3DS models using VRML renderer.
}

unit Object3dOpenGL;

interface

uses VectorMath, Object3dGEO, Object3dOBJ, Object3ds;

procedure RenderGEO(geo: TObject3dGEO);

{ Render OBJ model.

  Jesli useTexture2d to bedzie wlaczalo GL_TEXTURE_2D przed renderowaniem
  scian ktore maja HasTexCoords i bedzie genereowalo te tex coords
  i wylaczalo GL_TEXTURE_2D przed generowaniem sciany ktora nie ma HasTexCoords.
  Wpp. nie bedzie dotykalo ustawien tekstury, co umozliwi ci np. automatyczne
  generowanie wspolrzednych tekstury.

  Nawet jesli useTexture2d to sam musisz zaladowac odpowiednia teksture
  --- nie odczytujemy z .obj zadnych danych na temat tekstury.

  Zawsze generowane sa normale flat. }
procedure RenderOBJ(obj: TObject3dOBJ; useTexture2d: boolean);

{ Render 3DS model.

  Renderuje wszystkie Trimeshes uzywajac odpowiednich materialow.
  Zawartosc textury nr 0 (unnamed) OpenGLa moze zostac zmieniona
  jesli useTexture2d  }
procedure Render3DS(scene: TScene3ds; useTexture2d: boolean);

implementation

uses SysUtils, KambiUtils, GL, GLU, GLExt, KambiGLUtils, Object3dsMaterial, Images,
  GLImages;

procedure RenderGEO(geo: TObject3dGEO);
{ Renderujemy w naturalny sposor uzywajac vertex arrays. }
var i: integer;
begin
 with geo do begin
  glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);

  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVector3f), Verts.Items);

  glBegin(GL_TRIANGLES);
  for i := 0 to Faces.Count-1 do
  begin
   glNormalv(IndexedTriangleNormal(Faces.Items[i], PVector3Single(Verts.Items), 0));
   glArrayElement(Faces.Items[i][0]);
   glArrayElement(Faces.Items[i][1]);
   glArrayElement(Faces.Items[i][2]);
  end;
  glEnd;

  glPopClientAttrib;
 end;
end;

procedure RenderOBJ(obj: TObject3dOBJ; useTexture2d: boolean);
var i, j: integer;
begin
 with obj do begin
  glPushAttrib(GL_ENABLE_BIT);

  for i := 0 to Faces.Count-1 do
  with Faces.Items[i] do
  begin
   glNormalv(IndexedTriangleNormal(VertIndices, PVector3Single(Verts.Items), 0));

   if useTexture2d then setGLEnabled(GL_TEXTURE_2D, HasTexCoords);
   glBegin(GL_TRIANGLES);
   for j := 0 to 2 do
   begin
    if HasTexCoords and useTexture2d then
     glTexCoordv(TexCoords.Items[TexCoordIndices[j]]);
    glVertexv(Verts.Items[VertIndices[j]]);
   end;
   glEnd;
  end;

  glPopAttrib;
 end;
end;

{ 3ds rendering ------------------------------------------------------------ }

{ jezeli useTexture2d to ustawi sobie GL_TEXTURE_2D w zaleznosci
  od tego czy HasTexCoords i jezeli tak to bedzie sobie generowal
  odpowiednie tex coords. Wpp. nie bedzie dotykal ustawienia
  GL_TEXTURE_2D i tex coords (wiec tekstury powinny byc disabled /
  ew. mozesz generowac wspolrzedne tekstury automatycznie).
  Normale zawsze flat. }
procedure Render3DSTrimesh(Trimesh: TTrimesh3ds; useTexture2d: boolean);

  { atrybuty OpenGL'a ktore moga byc modyfikowane przez metode
    PrepareMaterial. Zanim uzyjesz PrepareMaterial mozesz zdecydowac
    sie na uzycie glPushAttrib(AttribsModified). }
  function Material3ds_AttribsModified(useTexture2d: boolean): TGLbitfield;
  begin
   result := GL_LIGHTING_BIT or GL_CURRENT_BIT;
   if useTexture2d then
    result := result or GL_TEXTURE_BIT or GL_ENABLE_BIT;
  end;

  { przygotuj odpowiedni stan OpenGL'a wynikajacy z ustawien materialu.
    Czyli wylacz GL_COLOR_MATERIAL, ustaw odpowiedni glColor i
    glMaterial na podstawie materiala,
    jezeli useTexture2d to ustaw enabled/disabled teksturom
    i ew. zaladuj odpowiednia teksture. }
  procedure Material3ds_PrepareMaterial(Material: TMaterial3ds; useTexture2d: boolean);

    procedure LoadTextureMap(const TexMap: TMaterialMap3ds);
    { wiedzac ze TexMap.Exists = true, sprobuj zaladowac teksture TexMap
      do domyslnej unnamed tekstury OpenGLa (nr 0).
      Jesli sie udalo ustaw GL_TEXTURE_2D enabled, wpp. disabled. }
    var Image: TImage;
    begin
     try
      Image := LoadImage(TexMap.MapFilename, GLImageClasses, []);
     except
      on E: Exception do begin
       WarningWrite('WARNING : Exception raised while trying to load texture "'+
         TexMap.MapFilename+ '" : ' +ExceptMessage(E, nil) +nl
         +'Turning this texture off.');
       glDisable(GL_TEXTURE_2D);
       Exit;
      end;
     end;
     try
      glEnable(GL_TEXTURE_2D);
      LoadGLGeneratedTexture(0, Image, GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR);
     finally FreeAndNil(Image) end;
    end;

  begin
   with Material do begin
    glDisable(GL_COLOR_MATERIAL);
    { inicjuj osobno glColor i glMaterial aby byc przygotowanym na
      renderowanie nas i w LIGHTING i bez lighting. }
    glColorv(DiffuseCol);
    glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, AmbientCol);
    glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, DiffuseCol);
    glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, SpecularCol);
    glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, Shininess * 128);

    if useTexture2d then
    begin
     if TextureMap1.Exists then
     begin
      LoadTextureMap(TextureMap1);

      {aplikuj odpowiednia macierz tekstury}
      { TODO: powinnismy tu jeszcze uzyc U/VOffset ale nie mam na to zadnego
        testu a nie wiem co robic najpierw - offset czy scale }
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
      glScalef(TextureMap1.UScale, TextureMap1.VScale, 1);
      glMatrixMode(GL_MODELVIEW);

     end else
      glDisable(GL_TEXTURE_2D);
    end;
   end;
  end;

  procedure Material3ds_PrepareDefaultMaterial(useTexture2d: boolean);
  begin
   glDisable(GL_COLOR_MATERIAL);
   { inicjuj osobno glColor i glMaterial aby byc przygotowanym na
     renderowanie nas i w LIGHTING i bez lighting. }
   glColorv(DefaultMatDiffuseColor4f);
   glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Default3dsMatAmbient);
   glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE, Default3dsMatDiffuse);
   glMaterialv(GL_FRONT_AND_BACK, GL_SPECULAR, Default3dsMatSpecular);
   glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, Default3dsMatShininess * 128);

   if useTexture2d then
    glDisable(GL_TEXTURE_2D);
  end;

var i, j: integer;
    UsedMaterial: integer;
begin
 with Trimesh do begin
  glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
  glPushAttrib(Material3ds_AttribsModified(useTexture2d));

  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TVertex3ds), @Verts^[0].Pos);
  if useTexture2d and HasTexCoords then
  begin
   {szczegoly inicjowania textury sa wykonywane w PrepareMaterial}
   glEnableClientState(GL_TEXTURE_COORD_ARRAY);
   glTexCoordPointer(2, GL_FLOAT, SizeOf(TVertex3ds), @Verts^[0].TexCoord);
  end;

  {we use UsedMaterial to record what material is currently prepared.
   It saves a LOT of time not to Prepare already prepared material -
   at least on mine NVidia. }
  Material3ds_PrepareDefaultMaterial(useTexture2d);
  UsedMaterial := -1;

  { cast to Integer before -1 because 0-1 => RangeError because FacesCount
    is of type Word (that is, it's unsigned) }
  for i := 0 to Integer(FacesCount)-1 do
  with Faces^[i] do
  begin
   if UsedMaterial <> FaceMaterialIndex then
   begin
    if FaceMaterialIndex <> -1 then
     Material3ds_PrepareMaterial(Scene.Materials[FaceMaterialIndex], useTexture2d) else
     Material3ds_PrepareDefaultMaterial(useTexture2d);
    UsedMaterial := FaceMaterialIndex;
   end;

   glBegin(GL_TRIANGLES);
     glNormalv(IndexedTriangleNormal(VertsIndices, @Verts^[0].Pos, SizeOf(TVertex3ds)));
     for j := 0 to 2 do
     begin
      if EdgeFlags[j] then glEdgeFlag(GL_TRUE) else glEdgeFlag(GL_FALSE);
      glArrayElement(VertsIndices[j]);
     end;
   glEnd;
  end;

  glPopClientAttrib;
  glPopAttrib;
 end;
end;

procedure Render3DS(Scene: TScene3ds; useTexture2d: boolean);

(* TODO:
{ ustaw parametry glLight* swiatla OpenGL'a numer glightnum
  (tzn. gllightnum = 0 oznacza GL_LIGHT0). Ustaw tez enabled/disabled tego
  swiatla. }
procedure TLight3ds.SetupLight(gllightnum: integer);
var thislight: TGLenum;
begin
 thislight := gllightnum+GL_LIGHT0;
 glLightv(thislight, GL_AMBIENT, Black4f);
 glLightv(thislight, GL_DIFFUSE, Col);
 glLightv(thislight, GL_SPECULAR, Col);
 glLightv(thislight, GL_POSITION, Vector4f(Pos));
 glLighti(thislight, GL_SPOT_CUTOFF, 180);
 glLighti(thislight, GL_CONSTANT_ATTENUATION, 1);
 glLighti(thislight, GL_LINEAR_ATTENUATION, 0);
 glLighti(thislight, GL_QUADRATIC_ATTENUATION, 0);
 SetGLEnabled(thislight, Enabled);
end;
*)

var i: Integer;
begin
 for i := 0 to Scene.Trimeshes.Count-1 do
  Render3DSTrimesh(Scene.Trimeshes[i], useTexture2d);
end;

end.
