{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels.gl Pascal units".

  "Kambi's 3dmodels.gl Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels.gl Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels.gl Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Wywolaj OnBeforeGLVertex jezeli jest <> nil,
  ustaw fog coordinate.

  v to wektor we wspolrzednych LOKALNYCH aktualnego Node'a -
  sami go sobie tutaj przemnozymy przez State.CurrMatrix. }
procedure TVRMLOpenGLRenderer.DoBeforeGLVertex(const Vert: TVector3Single);
var
  VertGlobal, VertProjected: TVector3Single;
  VertFogDistance: Single;
begin
  { We check first, to avoid calculating VertGlobal when it's not needed. }
  if Assigned(Attributes.OnBeforeGLVertex) or FogVolumetric then
  begin
    VertGlobal := MultMatrixPoint(Render_State.CurrMatrix, Vert);

    if FogVolumetric then
    begin
      VertProjected := PointOnLineClosestToPoint(
        ZeroVector3Single, FogVolumetricDirection, VertGlobal);
      VertFogDistance := VectorLen(VertProjected);
      if not AreParallelVectorsSameDirection(
        VertProjected, FogVolumetricDirection) then
        VertFogDistance := -VertFogDistance;
      { Now I want
        - VertFogDistance = FogVolumetricVisibilityStart -> 0
        - VertFogDistance = FogVolumetricVisibilityStart + X -> X
          (so that VertFogDistance = FogVolumetricVisibilityStart +
          FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
      VertFogDistance -= FogVolumetricVisibilityStart;
      glFogCoordfEXT(VertFogDistance);
    end;

    if Assigned(Attributes.OnBeforeGLVertex) then
      Attributes.OnBeforeGLVertex(Render_Node, VertGlobal);
  end;
end;

{ ZAWSZE wywoluj glVertex tylko za posrednictwem tych
  procedur - one zajma sie wywolaniem DoBeforeGLVertex z odpowiednio
  przetransformowanym v. Wersja GLArrayElement wywoluje glArrayElemnt(ith),
  i musisz podac tablice do ktorej vertexow odnosi sie ten index
  (to znaczy Verts[ith] powinno dac odpowiedni vertex;
   zeby wszystko dzialalo ok oczywiscie OpenGL musi miec zaladowana
   wlasnie ta tablice jako vertex array). }

procedure TVRMLOpenGLRenderer.DoGLVertex(const Vert: TVector3Single);
begin
 DoBeforeGLVertex(Vert);
 glVertexv(Vert);
end;

procedure TVRMLOpenGLRenderer.DoGLArrayElement(
  const Verts: PArray_Vector3Single; ith: TGLint);
begin
 DoBeforeGLVertex(Verts[ith]);
 glArrayElement(ith);
end;
