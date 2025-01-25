{%MainUnit castleglutils.pas}
{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: simple types. }

{$ifdef read_interface}

type
  { Types (invented in our engine) that correspond to WebGL objects,
    but map to simple types (like TGLuint) for OpenGL / OpenGLES. }
  { }
  TGLBuffer = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLBuffer {$endif};
  TGLFramebuffer = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLFramebuffer {$endif};
  TGLRenderbuffer = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLRenderbuffer {$endif};
  TGLProgram = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLProgram {$endif};
  TGLShader = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLShader {$endif};
  TGLTexture = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLTexture {$endif};
  TGLVertexArrayObject = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLVertexArrayObject {$endif};
  TGLQuery = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLQuery {$endif};

  { Uniform location in WebGL / OpenGL / OpenGLES.

    Note: This is *signed*, TGLint, deliberately.
    glGetUniformLocation returns -1 when uniform not found
    ( https://registry.khronos.org/OpenGL-Refpages/gl4/html/glGetUniformLocation.xhtml ),
    and our API in CastleGLShaders checks for it. }
  TGLUniformLocation = {$ifndef CASTLE_WEBGL} TGLint {$else} CastleInternalJobWeb.IJSWebGLUniformLocation {$endif};

  { Attribute location in WebGL / OpenGL / OpenGLES.

    Note: Attribute location is TGLint in both WebGL and OpenGL / OpenGLES and can be -1.
    See https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/getAttribLocation .
    So it's inconsistent with uniform location in WebGL.
    We define TGLAttribLocation and GLAttribLocationNone to make
    it look consistent. }
  TGLAttribLocation = TGLint;

const
  GLObjectNone = {$ifndef CASTLE_WEBGL} 0 {$else} nil {$endif};
  GLUniformLocationNone = {$ifndef CASTLE_WEBGL} -1 {$else} nil {$endif};
  GLAttribLocationNone = -1;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
