{
  Copyright 2008 Michalis Kamburelis.

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

{ TVRMLMeshRenderer descendants implementing nodes in X3D "Geometry2D"
  component. }

type
  TRectangle2DRenderer = class(TVRMLMeshRenderer)
  private
    Rectangle2D: TNodeRectangle2D;
  protected
    procedure DoRender; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TPlaneRenderer = class(TVRMLMeshRenderer)
  private
    Plane: TNodePlane;
  protected
    procedure DoRender; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TCircle2DRenderer = class(TVRMLMeshRenderer)
  protected
    procedure DoRender; override;
  end;

{ TRectangle2DRenderer -------------------------------------------------------- }

constructor TRectangle2DRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;
  Rectangle2D := Geometry as TNodeRectangle2D;

  CullBackFaces := Rectangle2D.FdSolid.Value;
  FrontFaceCcw := true;
end;

procedure TRectangle2DRenderer.DoRender;
var
  V: TVector3Single;
begin
  glBegin(GL_QUADS);
    glNormal3f(0, 0, 1);

    V[0] := -Rectangle2D.FdSize.Value[0] / 2;
    V[1] := -Rectangle2D.FdSize.Value[1] / 2;
    V[2] := 0;

    glTexCoord2f(0, 0);
    DoGLVertex(V);

    V[0] := -V[0];
    glTexCoord2f(1, 0);
    DoGLVertex(V);

    V[1] := -V[1];
    glTexCoord2f(1, 1);
    DoGLVertex(V);

    V[0] := -V[0];
    glTexCoord2f(0, 1);
    DoGLVertex(V);
  glEnd;
end;

{ TPlaneRenderer -------------------------------------------------------- }

constructor TPlaneRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;
  Plane := Geometry as TNodePlane;

  CullBackFaces := Plane.FdSolid.Value;
  FrontFaceCcw := true;
end;

procedure TPlaneRenderer.DoRender;
var
  V: TVector3Single;
begin
  glBegin(GL_QUADS);
    glNormal3f(0, 0, 1);

    V[0] := -Plane.FdSize.Value[0] / 2;
    V[1] := -Plane.FdSize.Value[1] / 2;
    V[2] := 0;

    glTexCoord2f(0, 0);
    DoGLVertex(V);

    V[0] := -V[0];
    glTexCoord2f(1, 0);
    DoGLVertex(V);

    V[1] := -V[1];
    glTexCoord2f(1, 1);
    DoGLVertex(V);

    V[0] := -V[0];
    glTexCoord2f(0, 1);
    DoGLVertex(V);
  glEnd;
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
