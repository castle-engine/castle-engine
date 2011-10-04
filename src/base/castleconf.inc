{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Define some useful symbols for conditional compilation.

  This is a central include file for our Castle Game Engine.
  It is not necessarily included in all of our units, however it's included
  in many of them --- always when something needs to check for given symbol.

  This file does not define some basic compiler settings, like
  syntax things (hugestrings ? which FPC mode to use ? ...) or what
  checks to do (io checks ? range checks ? assertions ? ...).
  Instead I depend on the fact that all my units must be compiled with
  my ../kambi.cfg configuration file. That file defines such basic
  compiler options. That's because I can't control everything using
  include file, like this one. E.g. I can't say here to add HeapTrc
  and LineInfo at the beginning of "uses" clause of current unit/program.
  But I can say it easily in ../kambi.cfg using "-gl -gh" options.
}

{$ifndef CASTLE_CONF_INCLUDED}
{$define CASTLE_CONF_INCLUDED}

{ Configurable: define CASTLE_ENGINE_LGPL to compile only
  components available on permissive LGPL (see
  http://castle-engine.sourceforge.net/engine.php#section_license) }
{ $define CASTLE_ENGINE_LGPL}

{ MSWINDOWS is the preferred symbol to mark Windows-only code
  (that is not necessarily tied only to 32-bit Windows).
  But older Delphi and FPC 1.0.x did not define this symbol, fix it. }
{$ifdef WIN32} {$define MSWINDOWS} {$endif}

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
{$ifdef FPC}
  {$ifndef FPC_HAS_TYPE_EXTENDED}
    {$define EXTENDED_EQUALS_DOUBLE}
  {$endif}
{$endif}

{$ifdef FPC}
  { We do *not* define inline functions/methods when compiling from
    Lazarus package. This is to workaround FPC bug
    http://bugs.freepascal.org/view.php?id=12223 }
  {$ifndef KAMBI_FROM_LAZARUS_PACKAGE}
    {$define SUPPORTS_INLINE}
  {$endif}

  {$ifdef VER2_0}   {$fatal FPC 2.0.x is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}
  {$ifdef VER2_2_0} {$fatal FPC 2.2.0 is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}
  {$ifdef VER2_2_2} {$fatal FPC 2.2.2 is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}

  {$define TOBJECT_HAS_EQUALS}
  {$ifdef VER2_0}   {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_2_0} {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_2_2} {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_2_4} {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_4_0} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$endif}

{$endif not CASTLE_CONF_INCLUDED}
