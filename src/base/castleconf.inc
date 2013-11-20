{
  Copyright 2002-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Common compiler defines and symbols.
  This is a central include file for Castle Game Engine.

  This file is *not* included in 100% of our units (for now),
  it's just included where it's needed.

  Note that we don't try to set compilation options here (like $H or FPC
  ObjFpc mode) because not all interesting compiler settings can be set
  by directives. And specyfying only a subset of them is useless.
  The proper compiler options to compile the engine are in ../../castle-fpc.cfg
  configuration file (used also by fpmake compilation method),
  and they are replicated in Lazarus packages compilation settings.
}

{$ifndef CASTLE_CONF_INCLUDED}
{$define CASTLE_CONF_INCLUDED}

{$ifndef FPC}
  {$error You have to use FPC (Free Pascal Compiler) to compile Castle Game Engine. The engine is not suitable for Delphi or other Pascal compilers. (But patches to change this are welcome. Michalis doesn't care about Delphi, but if someone else wants to step up and fix Delphi compatibility --- go for it.)}
{$endif}

{ Configurable: define CASTLE_ENGINE_LGPL to compile only
  components available on permissive LGPL (see
  http://castle-engine.sourceforge.net/engine.php#section_license) }
{ $define CASTLE_ENGINE_LGPL}

(*EXTENDED_EQUALS_DOUBLE should be defined when Extended type is
  the same thing as Double type on this platform.

  One typical case when this is important is when you overload
  one procedure like
    p(single)
    p(double)
    p(extended)
  In such cases you must do it like this:
    p(single)
    p(double)
    {$ifndef EXTENDED_EQUALS_DOUBLE} p(extended) {$endif}

  According to FPC docs (Programmers Manual, 8.2.0: floating point types),
  there's no Extended (i.e. Extended = Double) for most of non-i386 architectures.
  Exception to the above is Linux on x86-64, that allows to use normal Extended.
  Maybe Darwin on x86-64 also?
*)
{$ifndef FPC_HAS_TYPE_EXTENDED}
  {$define EXTENDED_EQUALS_DOUBLE}
{$endif}

{ We do *not* define inline functions/methods when compiling from
  Lazarus package. This is to workaround FPC bug
  http://bugs.freepascal.org/view.php?id=12223 }
{$ifndef CASTLE_ENGINE_LAZARUS_PACKAGE}
  {$define SUPPORTS_INLINE}
{$endif}

{$ifdef VER2_0} {$fatal FPC 2.0.x is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}
{$ifdef VER2_2} {$fatal FPC 2.2.x is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}
{$ifdef VER2_4} {$fatal FPC 2.4.x is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}

{$define TOBJECT_HAS_EQUALS}
{$ifdef VER2_0}   {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_2_0} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_2_2} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_2_4} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_4_0} {$undef TOBJECT_HAS_EQUALS} {$endif}

{$if defined(iPHONESIM) or (defined(DARWIN) and defined(CPUARM))}
    {$define iOS}
{$endif}

{ Whether we use OpenGL ES or normal OpenGL.
  OpenGL ES is generally for mobile devices.
  For testing, you can also use OpenGL ES on normal desktop OSes
  (under Debian, install libegl1-mesa-dev and libgles2-mesa-dev).
  Put "CastleGL" in uses clause to include the appropriate OpenGL API. }
{ $define OpenGLES}
{$ifdef ANDROID} {$define OpenGLES} {$endif}
{$ifdef iOS} {$define OpenGLES} {$endif}
{$ifdef OpenGLES}
  {$define CastleGL := CastleGLES20}
  {$define GLImageUseShaders}
{$else}
  {$define CastleGL := GL, GLU, GLExt}
  { It is possible to test GLImageUseShaders also with non-OpenGLES version.
    All the TGLImage and DrawRectangle rendering will go through shaders. }
  { $define GLImageUseShaders}
{$endif}

{ Whether we use a different version of CastlePng unit that allows
  static linking to png, see castlepng_static.inc. }
{ $define CASTLE_PNG_STATIC}

{$endif not CASTLE_CONF_INCLUDED}
