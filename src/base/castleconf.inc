{
  Copyright 2002-2017 Michalis Kamburelis.

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

  This file is *not* included in 100% of our units,
  it's just included where it's needed.
  We don't try to set compilation options here (like $H+ or FPC objfpc mode)
  because:
  - Not all interesting compiler settings can be set here.
    For example optimization options can only be specified on the command-line
    (and they are important for a game engine).
  - It's error prone, it's too easy to forget to include this file in some unit.
    Using a consistent compiler options in scripts is easier.

  The proper compiler options to compile the engine are
  - in the ../../castle-fpc.cfg configuration file (used also by fpmake
    compilation method),
  - and they are replicated in Lazarus packages compilation settings,
  - and are replicated once more in the castle-engine build tool code.
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
  http://bugs.freepascal.org/view.php?id=12223 . }
{$ifndef CASTLE_ENGINE_LAZARUS_PACKAGE}
  {$define SUPPORTS_INLINE}
{$endif}

{$ifdef VER2_0} {$fatal FPC 2.0.x is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}
{$ifdef VER2_2} {$fatal FPC 2.2.x is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}
{$ifdef VER2_4} {$fatal FPC 2.4.x is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}
{$ifdef VER2_6_0} {$fatal FPC 2.6.0 is not supported anymore, see http://castle-engine.sourceforge.net/engine.php#section_fpc_ver} {$endif}

{ These should be set on the command-line always.
  NOT setting them here forces the user to use correct compilation method
  for the engine (like using castle-fpc.cfg or the build tool or Lazarus package)
  instead of hacking something, which is GOOD. }
{ $mode objfpc}
{ $H+}
{ $macro on}

{ Unfortunately, it seems that this can only be specified here, not on the command-line. }
{$writeableconst off}
{$modeswitch advancedrecords}

{$define TOBJECT_HAS_EQUALS}
{$ifdef VER2_0}   {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_2_0} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_2_2} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_2_4} {$undef TOBJECT_HAS_EQUALS} {$endif}
{$ifdef VER2_4_0} {$undef TOBJECT_HAS_EQUALS} {$endif}

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below
  {$if defined(iPHONESIM) or (defined(DARWIN) and (defined(CPUARM) or defined(CPUARM64)))}
    {$define iOS}
  {$endif}
{$endif}

{ On some Android versions, you cannot call dlopen (load dynamic libraries)
  from the initialization section of units. You have to wait for
  AndroidMain to be called by NDK, otherwise we get a crash at initialization. }
{$define ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
{$ifdef ANDROID}
  {$undef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
{$endif}

{ OpenGL ES ------------------------------------------------------------------ }

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

  { This is necessary to workaround buggy OpenGLES on Android EMUI 3.1
    (Renderer = "Mali-450 MP", Vendor = "ARM", Version string "OpenGL ES 2.0").
    It does not accept GL_UNSIGNED_INT as index type for glDrawElements
    (yeah, pretty horrible...).

    Workaround means that we render using 16-bit indexes, which may also be useful
    to gain a little performance, *but* it means we cannot render large X3D shapes.
    The more long-term solution would be to detect the need for 32-bit indexes
    at runtime, and switch between 16-bit and 32-bit indexes as needed.
    This would allow performance gain on both desktops and mobiles (but I did not
    check whether this gain is noticeable!), while still allowing to render large
    shapes. Unless this specific buggy Android EMUI 3.1 device is detected,
    in which case we can
    - raise an exception
    - or force AllowIndexed := false in X3D renderer
    - or split the shapes (but this would complicate the rendering code)
  }
  {$define GLIndexesShort}
{$else}
  {$define CastleGL := GL, GLU, GLExt}
  { It is possible to test GLImageUseShaders also with non-OpenGLES version.
    All the TGLImageCore and DrawRectangle rendering will go through shaders. }
  { $define GLImageUseShaders}
{$endif}

{ PNG ------------------------------------------------------------------------ }

{ There are 3 ways to read/write the PNG format:
  - If CASTLE_PNG_USING_FCL_IMAGE is defined, then we use
    PNG implementation inside fcl-image, that does not depend on libpng API.
    This is the simplest option if you don't want the hassle of linking
    to libpng library (and distributing it, in case of dynamic linking).
    This is the default for now, as it makes distribution easiest,
    on all platforms (standalone, mobile...).
    Also, aggressive detection of when texture is grayscale/RGB
    and when it has alpha channel (or is opaque) right now only happens
    in this case, which helps reduce GPU usage.
    Also, libpng under Windows can raise some dumb antivirus (ZoneAlarm)
    warnings, see
    https://sourceforge.net/p/castle-engine/discussion/general/thread/2fd412b1/ .

  - Using libpng from a dynamic library (.so or .dll).
    On Linux and other desktop Unixes, this is pretty much standard.
    On Windows, see
    http://svn.code.sf.net/p/castle-engine/code/trunk/external_libraries/ .
    This is used when neither CASTLE_PNG_USING_FCL_IMAGE
    nor CASTLE_PNG_STATIC are defined.

  - If CASTLE_PNG_STATIC is defined, we use a different version of CastlePng
    unit that allows static linking to png, see castlepng_static.inc.
}

{$define CASTLE_PNG_USING_FCL_IMAGE}
// {$ifdef iOS} {$define CASTLE_PNG_USING_FCL_IMAGE} {$endif}
// {$ifdef ANDROID} {$define CASTLE_PNG_USING_FCL_IMAGE} {$endif}

{ Below is relevant only if we use dynamic PNG library (castlepng_dynamic.inc).
  Determines calling convention of libpng functions and callbacks:
  LIBPNG_STDCALL or LIBPNG_CDECL. The standard is to just define LIBPNG_CDECL,
  also for Windows: normal Windows dlls of libpng use cdecl, just like Unix. }
{$ifdef MSWINDOWS}
  {$define LIBPNG_CDECL}
{$else}
  {$define LIBPNG_CDECL}
{$endif}

{ FpHttpClient --------------------------------------------------------------- }

{ Keep HAS_FP_HTTP_CLIENT defined for all platforms that have
  FpHttpClient implemented. }
{$define HAS_FP_HTTP_CLIENT}
{$ifdef ANDROID} {$undef HAS_FP_HTTP_CLIENT} {$endif}

{ FreeType ------------------------------------------------------------------- }

{$define HAS_FREE_TYPE}
{$ifdef ANDROID} {$undef HAS_FREE_TYPE} {$endif}
{$ifdef iOS} {$undef HAS_FREE_TYPE} {$endif}

{ Embedded stuff ------------------------------------------------------------- }

{ If defined, the compiled code will have embedded all the variations
  of basic fonts for the X3D Text node
  (see [http://castle-engine.sourceforge.net/x3d_implementation_text.php]).
  This is comfortable (the FontStyle font works, with 3 * 4 font variations,
  out of the box), but it increases your exe size --- possibly without a reason,
  if you don't use FontStyle X3D node.

  Undefine this to conserve around 0.7 MB in exe size.
  All the Text X3D nodes will use standard sans non-bold, non-italic font. }
{$define CASTLE_EMBED_ALL_3D_FONT_VARIATIONS}

{$endif not CASTLE_CONF_INCLUDED}
