{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Boxes and OpenGL. }
unit CastleGLBoxes deprecated 'this unit no longer contains anything useful; use TCastleBox to draw boxes';

{$I castleconf.inc}

interface

uses CastleBoxes;

{$ifndef OpenGLES}

{ Draw the wireframe box.
  Nothing is generated besides vertex positions ---
  no normal vectors, no texture coords, nothing. }
procedure glDrawBox3DWire(const Box: TBox3D); deprecated 'do not draw like this; instead use TCastleBox or TCastleScene with TBoxNode';

{$endif}

implementation

uses {$ifdef FPC} CastleGL {$else} GL, GLExt {$endif};

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
  glDrawRaw(
    Box.Data[0].X, Box.Data[0].Y, Box.Data[0].Z,
    Box.Data[1].X, Box.Data[1].Y, Box.Data[1].Z)
end;

{$endif}

end.
