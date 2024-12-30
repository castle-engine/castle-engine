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
    but maps to simple types (like TGLuint) for OpenGL / OpenGLES. }
  TGLbuffer = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLBuffer {$endif};
  TGLFramebuffer = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLFramebuffer {$endif};
  TGLProgram = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLProgram {$endif};
  TGLShader = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLShader {$endif};
  TGLTexture = {$ifndef CASTLE_WEBGL} TGLuint {$else} CastleInternalJobWeb.IJSWebGLTexture {$endif};
  TGLUniformLocation = {$ifndef CASTLE_WEBGL} TGLint {$else} CastleInternalJobWeb.IJSWebGLUniformLocation {$endif};

const
  GLbufferNone = {$ifndef CASTLE_WEBGL} 0 {$else} nil {$endif};

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
