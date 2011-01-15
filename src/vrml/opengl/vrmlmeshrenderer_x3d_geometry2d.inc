{
  Copyright 2008-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TVRMLMeshRenderer descendants implementing nodes in X3D "Geometry2D"
  component. }

type
  TCircle2DRenderer = class(TVRMLMeshRenderer)
  protected
    procedure DoRender; override;
  end;

{ TCircle2DRenderer ---------------------------------------------------------- }

procedure TCircle2DRenderer.DoRender;
var
  Slices: Integer;
  Circle2D: TNodeCircle2D;
  I: Integer;
  AngleSin, AngleCos: Float;
  V: TVector3Single;
begin
  Circle2D := Geometry as TNodeCircle2D;
  Slices := State.LastNodes.KambiTriangulation.QuadricSlices;

  glBegin(GL_LINE_LOOP);
    for I := 0 to Slices - 1 do
    begin
      SinCos((I / Slices) * 2 * Pi, AngleSin, AngleCos);
      V[0] := AngleSin * Circle2D.FdRadius.Value;
      V[1] := AngleCos * Circle2D.FdRadius.Value;
      V[2] := 0;

      { I have no idea is this default texture mapping (it maps
        texture on the circle, just like radius would be 1)
        is good. X3D spec doesn't say how to generate tex coords for this. }
      glTexCoord2f(AngleSin, AngleCos);
      DoGLVertex(V);
    end;
  glEnd;
end;
