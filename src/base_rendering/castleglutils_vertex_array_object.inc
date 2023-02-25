{%MainUnit castleglutils.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Vertex array object.
    Thin wrapper over VAO, as defined by OpenGL >= 3.
    Always create, use and destroy it in the same OpenGL(ES) context.

    This does nothing if current OpenGL(ES) version doesn't support VAO.

    Change the current VAO by @link(TRenderContext.CurrentVao RenderContext.CurrentVao).
    It is done done automatically by @link(TGLSLAttribute.EnableArray),
    remember that VBOs (including GL_ELEMENT_ARRAY_BUFFER) must be bound within this VAO. }
  TVertexArrayObject = class
  strict private
    FInternalHandle: TGLuint;
  public
    constructor Create;
    destructor Destroy; override;
    { OpenGL VAO identifier. }
    property InternalHandle: TGLuint read FInternalHandle;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TVertexArrayObject.Create;
begin
  inherited;
  if GLFeatures.VertexArrayObject then
    glGenVertexArrays(1, @FInternalHandle);
end;

destructor TVertexArrayObject.Destroy;
begin
  glFreeVertexArray(FInternalHandle);
  inherited;
end;

{$endif read_implementation}
