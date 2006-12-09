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

      { When VertFogDistance < 0 our intention is to have no fog.
        So VertFogDistance < 0 should be equivalent to VertFogDistance = 0.
        However, OpenGL doesn't necessarily interpret it like this.

        Since factor given by glFogCoordfEXT is interpreted just like
        eye distance (i.e. it's processed by appopriate linear or exp or exp2
        equations), negative values may produce quite unexpected results
        (unless you really look at the equations).

        This is mentioned in the extension specification
        [http://oss.sgi.com/projects/ogl-sample/registry/EXT/fog_coord.txt].
        First is says:

          * Should the specified value be used directly as the fog weighting
            factor, or in place of the z input to the fog equations?

    	    As the z input; more flexible and meets ISV requests.

        ... which means that what glFogCoordfEXT gives is interpreted
        just like eye distance for normal fog (so it's e.g. affected
        by fog linear / exp / exp2 modes, affected by fog start and end values,
        etc.). Later it says:

          * Should the fog coordinate be restricted to non-negative values?

            Perhaps. Eye-coordinate distance of fragments will be
            non-negative due to clipping. Specifying explicit negative
            coordinates may result in very large computed f values, although
            they are defined to be clipped after computation.

        ... and this is precisely why specifying negative glFogCoordfEXT
        parameters is a bad idea: you don't really know what OpenGL
        implementation will do. NVidia OpenGL seems to actually assume
        that factor < 0 means the same as factor = 0, so my code
        worked OK without this "MaxTo1st(VertFogDistance, 0);" below
        (because NVidia OpenGL was actually doing it anyway).
        Mesa 3D (and Radeon, as I suspect, because similar problems
        were reported for "The Castle" on Radeon) seem to just use the negative
        value directly, which causes strange artifacts
        (see e.g. "The Castle" gate_final.wrl VRML file).

        The line "MaxTo1st(VertFogDistance, 0);" makes volumetric fog work
        OK as expected for all OpenGL implementations. }
      MaxTo1st(VertFogDistance, 0);
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
 DoBeforeGLVertex(Verts^[ith]);
 glArrayElement(ith);
end;
