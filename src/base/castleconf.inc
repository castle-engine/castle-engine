{
  Copyright 2002-2018 Michalis Kamburelis.

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

{ Compiler version check ----------------------------------------------------- }

{$ifndef FPC}
  // See http://delphi.wikia.com/wiki/CompilerVersion_Constant
  {$if CompilerVersion < 20}
    {$error Use FPC (Free Pascal Compiler) or Delphi >= 2009 to compile Castle Game Engine. The older Delphi versions are not supported (we need generics).}
  {$endif}
{$endif}

{$ifdef VER2}
  {$fatal You need FPC version >= 3.0.2 to compile Castle Game Engine. See https://castle-engine.io/supported_compilers.php}
{$endif}
{$ifdef VER3_0_0}
  {$fatal You need FPC version >= 3.0.2 to compile Castle Game Engine. See https://castle-engine.io/supported_compilers.php}
{$endif}

{ Configure syntax ----------------------------------------------------------- }

{ These should be set on the command-line always.
  - NOT setting them here forces the user to use correct compilation method
    for the engine (like using castle-fpc.cfg or the build tool or Lazarus package)
    instead of hacking something, which is GOOD.
  - Also, not setting them allows to check compilation of the engine without macros
    and in Delphi mode, which is useful to keep Delphi compatibility.
}
{ $mode objfpc}
{ $H+}
{ $macro on}

{ For FPC, these can only be specified here, not on the FPC command-line. }
{$writeableconst off}
{$ifdef FPC} {$modeswitch advancedrecords} {$endif}

{ Define various necessary things -------------------------------------------- }

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

{ We used to disable inline functions/methods when compiling from
  Lazarus package (when CASTLE_ENGINE_LAZARUS_PACKAGE),
  to workaround FPC bug http://bugs.freepascal.org/view.php?id=12223 .
  That bug is closed, but with an answer basically "it will never be fixed",
  which means that you cannot have units in Lazarus package having
  "inline". (Unless you add -Ur, which would be uncomfortable for
  engine development from Lazarus.)

  But with FPC 3.0.2 everything seems to work cool even with "inline",
  so let's try defining it always now.
  We only support FPC >= 3.0.0 now.
}
{$define SUPPORTS_INLINE}

{$define TOBJECT_HAS_EQUALS}

{ Define CPU32/64, like FPC. }
{$ifndef FPC}
  {$ifdef CPUX86} {$define CPU32} {$endif}
  {$ifdef CPU32BITS} {$define CPU32} {$endif}
  {$ifdef CPUX64} {$define CPU64} {$endif}
  {$ifdef CPU64BITS} {$define CPU64} {$endif}
{$endif}

{ Traditionally, CGE defined CPUARM64 also for 64-bit iOS.
  For compatibility, keep it defined there.
  But CPUARM64 should not be used anymore: we use now CPUAARCH64,
  defined by FPC automatically (so it's safer to use, it will work
  regardless of how you compile the sources). }
{$ifdef CPUAARCH64}
  {$define CPUARM64}
{$endif}

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below
  {$if defined(iPHONESIM) or (defined(DARWIN) and (defined(CPUARM) or defined(CPUAARCH64)))}
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

// This serves two purposes:
// - Declaring something like TVector3.Zero (constant filled with zeros,
//   inside a record, which type is the same record)
//   causes internal error in FPC 3.0.2, 3.0.4 (not in 3.0.0 or 3.1.1,
//   so already fixed in FPC).
// - Declaring something like TVector3.Zero or TVector3.One
//   (any constant inside a record, which type is the same record)
//   is not allowed in Delphi, causes error that type is not completely defined.
{$ifdef FPC}
  {$define ENABLE_SELF_RECORD_CONSTANTS}
  {$ifdef VER3_0}
    {$ifndef VER3_0_0}
      {$undef ENABLE_SELF_RECORD_CONSTANTS}
    {$endif}
  {$endif}
{$endif}

// Workaround FPC 3.0.0 bug (not present in 3.0.2, 3.0.4, 3.1.1 -- so already fixed).
{$ifdef VER3_0_0}
  {$define BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}
{$endif}

// Allows to workaround FPC bug in some units, FPC seems to "lose"
// the fact that FPC_OBJFPC should be defined.
// The cleanest solution seems to just ignore FPC_OBJFPC,
// and define equivalent symbol (CASTLE_OBJFPC) on our own.
{$ifdef FPC}
  {$ifndef CASTLE_ENGINE_TEST_DELPHI_MODE}
    {$define CASTLE_OBJFPC}
  {$endif}
{$endif}

{ OpenGL[ES] ------------------------------------------------------------------ }

{ Whether we use OpenGL ES or normal OpenGL.
  OpenGL ES is generally for mobile devices.
  For testing, you can also use OpenGL ES on normal desktop OSes
  (under Debian, install libegl1-mesa-dev and libgles2-mesa-dev).
  Put "CastleGL" in uses clause to include the appropriate OpenGL API. }
{ $define OpenGLES}
{$ifdef ANDROID} {$define OpenGLES} {$endif}
{$ifdef iOS} {$define OpenGLES} {$endif}
{$ifdef OpenGLES}
  {$ifdef CASTLE_ENGINE_TEST_DELPHI_MODE}
    {$error We do not support Android/iOS for Delphi yet, use FPC/Lazarus to develop for Android/iOS and other OpenGLES platforms}
  {$else}
    {$define CastleGL := CastleGLES20}
  {$endif}

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
  { In our "Delphi test mode", macros are disabled, just like in actual Delphi. }
  {$ifndef CASTLE_ENGINE_TEST_DELPHI_MODE}
    {$define CastleGL := GL, GLU, GLExt}
  {$endif}
{$endif}

{ Define ForceStandardGLSLApi to only use GLSL shaders when included in OpenGL core
  (or not use GLSL shaders at all),
  and never use GLSL ARB extensions available on some OpenGL 1.x implementations. }
{$ifdef OpenGLES}
  {$define ForceStandardGLSLApi}
{$endif}
{$ifdef DARWIN}
  {$ifdef CPU64}
    (*Apple did something really weird, and defined GLhandleARB to be
      a pointer-size (not an GLint-size) type.

      See the (correct) definition in fpc/trunk/packages/opengl/src/glext.pp:
        {$ifdef DARWIN}
        GLHandleARB = Pointer;              // defined as void * in OpenGL.framework/glext.h
        {$else}
        GLhandleARB = Cardinal;
        {$endif}
      This is correct, i.e. this is consistent with C OpenGL header,
      and (apparently, if you believe the Internet) with what binary code expects:
      on macOS 64-bit, GLhandleARB is 8 bytes, not 4.

      To confuse matters more, some GLExt functions do not take GLhandleARB as they should,
      for example glGetProgramivARB (that takes GLEnum).
      So I'm unsure whether everything will really work correctly then -- one has to be extra
      careful when the GLhandleARB and GLint are (binary) different things *only on this one
      specific OS + architecture*.

      See others being baffled by this:
      - https://www.opengl.org/discussion_boards/showthread.php/175353-Scared-of-ARB-s-glext-h
      - http://irrlicht.sourceforge.net/forum/viewtopic.php?t=44069
      - http://trac.wildfiregames.com/ticket/1197

      This makes things uncomfortable in CastleGLShaders, as we cannot cast "ProgramId: TGLuint"
      to GLhandleARB safely. For simplicity, we just avoid using GLhandleARB,
      and whole family of ARB functions, on macOS 64-bit.
      They should not be needed in practice --- modern OpenGL versions
      don't need the ARB extensions to access the shaders. *)
    {$define ForceStandardGLSLApi}
  {$endif}
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

  - If CASTLE_PNG_DYNAMIC is defined,
    we use libpng from a dynamic library (.so or .dll).
    This is a bit harder to set up for developer, but may result in drastically
    faster PNG loading (even 4x times).

    - On Linux and other desktop Unixes, libpng is almost definitely already
      installed system-wide, so this just works.

    - On Windows, the appropriate dll is available in our build tool data in
      ../../tools/build-tool/data/external_libraries/ .

      It will be added to your Windows package by the build tool automatically when
      you run "castle-engine package ...".
      See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

      For development, you may want to copy the DLLs from
      ../../tools/build-tool/data/external_libraries/ to your project exe dir,
      or to place these DLLs in a directory listed on the PATH environment variable.

    - For macOS, see https://castle-engine.io/macosx_requirements.php .

  - If CASTLE_PNG_STATIC is defined, we use a different version of CastleInternalPng
    unit that allows static linking to png, see castleinternalpng_static.inc.
}

{$ifdef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below
  {$define CASTLE_PNG_USING_FCL_IMAGE}
{$else}
  // iOS or Android support only CASTLE_PNG_USING_FCL_IMAGE
  // (they could work with other methods too, but it needs some work to set up,
  // so we don't support it officially)
  {$if defined(iOS) or defined(ANDROID)}
    {$undef CASTLE_PNG_DYNAMIC}
    {$undef CASTLE_PNG_STATIC}
    {$define CASTLE_PNG_USING_FCL_IMAGE}
  {$endif}
{$endif}

{$ifndef CASTLE_PNG_DYNAMIC}
  {$ifndef CASTLE_PNG_STATIC}
    // Automatically define CASTLE_PNG_USING_FCL_IMAGE, unless one of the other defines is set.
    // This allows developers to override this by setting e.g. -dCASTLE_PNG_DYNAMIC in ~/.fpc.cfg
    {$define CASTLE_PNG_USING_FCL_IMAGE}
  {$endif}
{$endif}

{ Below is relevant only if we use dynamic PNG library (castleinternalpng_dynamic.inc).
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
  FpHttpClient implemented, if you use FPC. }
{$define HAS_FP_HTTP_CLIENT}
{$ifdef ANDROID} {$undef HAS_FP_HTTP_CLIENT} {$endif}
{$ifndef FPC} {$undef HAS_FP_HTTP_CLIENT} {$endif}

{ Embedded stuff ------------------------------------------------------------- }

{ If defined, the compiled code will have embedded all the variations
  of basic fonts for the X3D Text node
  (see [https://castle-engine.io/x3d_implementation_text.php]).
  This is comfortable (the FontStyle font works, with 3 * 4 font variations,
  out of the box), but it increases your exe size --- possibly without a reason,
  if you don't use FontStyle X3D node.

  Undefine this to conserve around 0.7 MB in exe size.
  All the Text X3D nodes will use standard sans non-bold, non-italic font. }
{$define CASTLE_EMBED_ALL_3D_FONT_VARIATIONS}

{$endif not CASTLE_CONF_INCLUDED}
