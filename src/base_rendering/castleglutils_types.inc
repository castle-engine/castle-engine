{
  Copyright 2001-2018 Michalis Kamburelis.

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
  { Types with leading "T" } { }
  TGLenum     = GLenum;
  TGLboolean  = GLboolean;
  TGLbitfield = GLbitfield;
  TGLbyte     = GLbyte;
  TGLshort    = GLshort;
  TGLint      = GLint;
  TGLsizei    = GLsizei;
  TGLubyte    = GLubyte;
  TGLushort   = GLushort;
  TGLuint     = GLuint;
  TGLfloat    = GLfloat;
  TGLclampf   = GLclampf;
  {$ifndef OpenGLES}
  TGLdouble   = GLdouble;
  TGLclampd   = GLclampd;
  {$endif not OpenGLES}

{ OpenGL vector/matrix types ------------------------------------------------- }

type
  { }
  TVector2f = TVector2 deprecated 'use TVector2';
  PVector2f = PVector2 deprecated 'use PVector2';

  TVector2ub = TVector2Byte deprecated 'use TVector2Byte';
  PVector2ub = PVector2Byte deprecated 'use PVector2Byte';

  TVector2i = TVector2Integer deprecated 'use TVector2Integer';
  PVector2i = PVector2Integer deprecated 'use PVector2Integer';

  TVector3f = TVector3 deprecated 'use TVector3';
  PVector3f = PVector3 deprecated 'use PVector3';

  TVector3ub = TVector3Byte deprecated 'use TVector3Byte';
  PVector3ub = PVector3Byte deprecated 'use PVector3Byte';

  TVector3i = TVector3Integer deprecated 'use TVector3Integer';
  PVector3i = PVector3Integer deprecated 'use PVector3Integer';

  TVector4f = TVector4 deprecated 'use TVector4';
  PVector4f = PVector4 deprecated 'use PVector4';

  TVector4ub = TVector4Byte deprecated 'use TVector4Byte';
  PVector4ub = PVector4Byte deprecated 'use PVector4Byte';

  TVector4i = TVector4Integer deprecated 'use TVector4Integer';
  PVector4i = PVector4Integer deprecated 'use PVector4Integer';

  TMatrix2f = TMatrix2 deprecated 'use TMatrix2';
  PMatrix2f = PMatrix2 deprecated 'use PMatrix2';

  TMatrix3f = TMatrix3 deprecated 'use TMatrix3';
  PMatrix3f = PMatrix3 deprecated 'use PMatrix3';

  TMatrix4f = TMatrix4 deprecated 'use TMatrix4';
  PMatrix4f = PMatrix4 deprecated 'use PMatrix4';

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
