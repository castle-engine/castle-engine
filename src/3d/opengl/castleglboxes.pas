{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Boxes and OpenGL. }
unit CastleGLBoxes;

{$I castleconf.inc}

interface

uses CastleBoxes;

{$ifndef OpenGLES}

{ Draw the wireframe box.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawBox3DWire(const Box: TBox3D); deprecated 'do not draw like this, instead create TCastleScene with Cube node inside';

{$endif}

implementation

uses CastleGL;

{$ifndef OpenGLES}

procedure glDrawBox3DWire(const Box: TBox3D);

  procedure glDrawRaw(const minx, miny, minz, maxx, maxy, maxz: Single);
  begin
    glBegin(GL_LINE_LOOP);
      glVertex3f(minx, miny, minz);
      glVertex3f(maxx, miny, minz);
      glVertex3f(maxx, maxy, minz);
      glVertex3f(minx, maxy, minz);
      glVertex3f(minx, maxy, maxz);
      glVertex3f(maxx, maxy, maxz);
      glVertex3f(maxx, miny, maxz);
      glVertex3f(minx, miny, maxz);
    glEnd;

    glBegin(GL_LINES);
      glVertex3f(minx, miny, minz);
      glVertex3f(minx, maxy, minz);
      glVertex3f(minx, miny, maxz);
      glVertex3f(minx, maxy, maxz);
      glVertex3f(maxx, miny, minz);
      glVertex3f(maxx, miny, maxz);
      glVertex3f(maxx, maxy, minz);
      glVertex3f(maxx, maxy, maxz);
    glEnd;
  end;

begin
  glDrawRaw(Box.Data[0,0], Box.Data[0,1], Box.Data[0,2],
            Box.Data[1,0], Box.Data[1,1], Box.Data[1,2])
end;

{$endif}

end.
