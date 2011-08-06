{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This file defines many symbols that I use in my sources.

  I *do not* have to include it in all my units, I include it only when
  I use some particular symbol (e.g. DELPHI) in my unit. Then I want to include
  this file to make sure that such symbol is correctly defined
  (e.g. that DELPHI is defined if and only if I'm compiling with Delphi).

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

{$ifndef KambiConf_ALREADY_INCLUDED}
{$define KambiConf_ALREADY_INCLUDED}

{ Configurable: define KAMBI_VRMLENGINE_LGPL to compile only
  components available on permissive LGPL (see
  http://vrmlengine.sourceforge.net/kambi_vrml_game_engine.php#section_license) }
{ $define KAMBI_VRMLENGINE_LGPL}

{ According to Borland starting from Delphi 6 MSWINDOWS
  is the preferred symbol to mark Windows-only code
  (that is not necessarily tied only to 32-bit Windows).
  But older Delphi and FPC 1.0.x did not define this symbol,
  I fix this below. }
{$ifdef WIN32} {$define MSWINDOWS} {$endif}

{ A hack to detect Kylix 1, that AFAIK does not define any VERxxx }
{$IFNDEF FPC} {$ifdef LINUX} {$define VER140} {$endif} {$endif}

{ Symbols to check that we're compiled with Delphi (DELPHI symbol)
  and Delphi version.

  Note that Delphi 6 = Kylix 1. To differentiate between Delphi on Windows
  and Kylix use MSWINDOWS / LINUX symbols. }
{$ifdef VER80}  {$define DELPHI}                                                                                     {$endif}
{$ifdef VER90}  {$define DELPHI}                                                                                     {$endif}
{$ifdef VER93}  {$define DELPHI}                                                                                     {$endif}
{$ifdef VER100} {$define DELPHI}                                                                                     {$endif}
{$ifdef VER110} {$define DELPHI}                                                                                     {$endif}
{$ifdef VER120} {$define DELPHI} {$define DELPHI4_UP}                                                                {$endif}
{$ifdef VER125} {$define DELPHI} {$define DELPHI4_UP}                                                                {$endif}
{$ifdef VER130} {$define DELPHI} {$define DELPHI4_UP} {$define DELPHI5_UP}                                           {$endif}
{$ifdef VER140} {$define DELPHI} {$define DELPHI4_UP} {$define DELPHI5_UP} {$define DELPHI6_UP}                      {$endif}
{$ifdef VER150} {$define DELPHI} {$define DELPHI4_UP} {$define DELPHI5_UP} {$define DELPHI6_UP} {$define DELPHI7_UP} {$endif}

{ Borland does not define UNIX symbol, but I often use it. }
{$ifdef DELPHI} {$ifdef LINUX} {$define UNIX} {$endif} {$endif}

{ Does compiler support various things ?
  DEFPARS - Default parameters,
  SUPPORTS_INTERFACE - interfaces }
{$ifdef DELPHI4_UP}
  {$define DEFPARS}
  {$define SUPPORTS_INTERFACE}
{$endif}
{$ifdef FPC}
  {$ifndef VER1_0}
    {$define DEFPARS}
    {$define SUPPORTS_INTERFACE}
  {$endif}
{$endif}

{ I don't like Delphi warnings that "faXxx is specific to platform" }
{$ifdef DELPHI} {$warn SYMBOL_PLATFORM OFF} {$endif}

{ Always define USE_LIBC under FPC 1.0.x under UNIX,
  see README.use_libc }
{$ifdef FPC} {$ifdef VER1_0} {$ifdef UNIX}
  {$define USE_LIBC}
{$endif} {$endif} {$endif}

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

  {$ifdef VER2_0}   {$fatal FPC 2.0.x is not supported anymore, see http://vrmlengine.sourceforge.net/kambi_vrml_game_engine.php#section_fpc_ver} {$endif}
  {$ifdef VER2_2_0} {$fatal FPC 2.2.0 is not supported anymore, see http://vrmlengine.sourceforge.net/kambi_vrml_game_engine.php#section_fpc_ver} {$endif}
  {$ifdef VER2_2_2} {$fatal FPC 2.2.2 is not supported anymore, see http://vrmlengine.sourceforge.net/kambi_vrml_game_engine.php#section_fpc_ver} {$endif}

  {$define TOBJECT_HAS_EQUALS}
  {$ifdef VER2_0}   {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_2_0} {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_2_2} {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_2_4} {$undef TOBJECT_HAS_EQUALS} {$endif}
  {$ifdef VER2_4_0} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$endif}

{$endif not KambiConf_ALREADY_INCLUDED}
