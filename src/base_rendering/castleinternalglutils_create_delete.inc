{%MainUnit castleinternalglutils.pas}
{
  Copyright 2016-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: create/delete objects.

  These routines abstract away one of the differences between
  OpenGL(ES) and WebGL:

  - OpenGL and OpenGLES expose routines like glGenBuffers, glDeleteBuffers.
    They allow to create/delete multiple GL "objects" and take address of
    the array.

  - WebGL exposes routines like createBuffer, deleteBuffer.
    They allow to create/delete one at a time.

  This include file provides WebGL-like routines for OpenGL and OpenGLES.
  So we follow WebGL conventions, because:
  - they are simpler,
  - type-safe (no passing of pointers),
  - map to the usual usage (we almost always create / delete one object at a time),
  - for shaders and programs, OpenGL(ES) and WebGL already use the same
    conventions (like createShader, deleteShader).
    So it seems the shader API already "favors" the WebGL conventions.

  We also add FreeXxx routines, that make sure to
  - check is object GLObjectNone before freeing
  - set the object to GLObjectNone after freeing.
  These are similar (in concept) to FreeAndNil for Pascal objects.
}

{$ifdef read_interface}

{$ifndef CASTLE_WEBGL}
{ Create TGLBuffer using OpenGL / OpenGLES / WebGL. }
function glCreateBuffer: TGLBuffer;
{ Create TGLVertexArrayObject using OpenGL / OpenGLES / WebGL. }
function glCreateVertexArray: TGLVertexArrayObject;
{ Create TGLTexture using OpenGL / OpenGLES / WebGL. }
function glCreateTexture: TGLTexture;
{ Create TGLRenderbuffer using OpenGL / OpenGLES / WebGL. }
function glCreateRenderbuffer: TGLRenderbuffer;
{ Create TGLFramebuffer using OpenGL / OpenGLES / WebGL. }
function glCreateFramebuffer: TGLFramebuffer;
{ Delete TGLQuery using OpenGL / OpenGLES / WebGL. }
function glCreateQuery: TGLQuery;

procedure glDeleteBuffer(const Buffer: TGLBuffer);
procedure glDeleteVertexArray(const Vao: TGLVertexArrayObject);
procedure glDeleteTexture(const Texture: TGLTexture);
procedure glDeleteRenderbuffer(const Buf: TGLRenderbuffer);
procedure glDeleteFramebuffer(const Buf: TGLFramebuffer);
procedure glDeleteQuery(const Query: TGLQuery);
{$endif}

{ Free TGLBuffer, setting it to GLObjectNone. }
procedure FreeBuffer(var Buffer: TGLBuffer);
{ Free TGLVertexArrayObject, setting it to GLObjectNone. }
procedure FreeVertexArray(var Vao: TGLVertexArrayObject);
// In CastleGLImages:
// procedure FreeTexture(var Texture: TGLTexture);
{ Free TGLRenderbuffer, setting it to GLObjectNone. }
procedure FreeRenderbuffer(var Buf: TGLRenderbuffer);
{ Free TGLFramebuffer, setting it to GLObjectNone. }
procedure FreeFramebuffer(var Buf: TGLFramebuffer);
{ Free TGLQuery, setting it to GLObjectNone. }
procedure FreeQuery(var Query: TGLQuery);

{$endif read_interface}

{$ifdef read_implementation}

{$ifndef CASTLE_WEBGL}
function glCreateBuffer: TGLBuffer;
begin
  glGenBuffers(1, @Result);
end;

function glCreateVertexArray: TGLVertexArrayObject;
begin
  glGenVertexArrays(1, @Result);
end;

function glCreateTexture: TGLTexture;
begin
  glGenTextures(1, @Result);
end;

function glCreateRenderbuffer: TGLRenderbuffer;
begin
  glGenRenderbuffers(1, @Result);
end;

function glCreateFramebuffer: TGLFramebuffer;
begin
  glGenFramebuffers(1, @Result);
end;

function glCreateQuery: TGLQuery;
begin
  glGenQueries(1, @Result);
end;

procedure glDeleteBuffer(const Buffer: TGLBuffer);
begin
  glDeleteBuffers(1, @Buffer);
end;

procedure glDeleteVertexArray(const Vao: TGLVertexArrayObject);
begin
  glDeleteVertexArrays(1, @Vao);
end;

procedure glDeleteTexture(const Texture: TGLTexture);
begin
  glDeleteTextures(1, @Texture);
end;

procedure glDeleteRenderbuffer(const Buf: TGLRenderbuffer);
begin
  glDeleteRenderbuffers(1, @Buf);
end;

procedure glDeleteFramebuffer(const Buf: TGLFramebuffer);
begin
  glDeleteFramebuffers(1, @Buf);
end;

procedure glDeleteQuery(const Query: TGLQuery);
begin
  glDeleteQueries(1, @Query);
end;
{$endif}

procedure FreeBuffer(var Buffer: TGLBuffer);
begin
  if Buffer <> GLObjectNone then
  begin
    glDeleteBuffer(Buffer);
    Buffer := GLObjectNone;
  end;
end;

procedure FreeVertexArray(var Vao: TGLVertexArrayObject);
begin
  if Vao <> GLObjectNone then
  begin
    glDeleteVertexArray(Vao);
    Vao := GLObjectNone;
  end;
end;

procedure FreeRenderbuffer(var Buf: TGLRenderbuffer);
begin
  if Buf <> GLObjectNone then
  begin
    if GLFeatures.Framebuffer then
      glDeleteRenderbuffer(Buf);
    Buf := GLObjectNone;
  end;
end;

procedure FreeFramebuffer(var Buf: TGLFramebuffer);
begin
  if Buf <> GLObjectNone then
  begin
    if GLFeatures.Framebuffer then
      glDeleteFramebuffer(Buf);
    Buf := GLObjectNone;
  end;
end;

procedure FreeQuery(var Query: TGLQuery);
begin
  if Query <> GLObjectNone then
  begin
    glDeleteQuery(Query);
    Query := GLObjectNone;
  end;
end;

{$endif read_implementation}