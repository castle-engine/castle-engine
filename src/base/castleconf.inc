{
  Copyright 2002-2019 Michalis Kamburelis.

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

  The proper compiler options to compile the engine are:
  - in the ../../castle-fpc.cfg configuration file (used also by fpmake
    compilation method),
  - and they are replicated in Lazarus packages compilation settings,
  - and are replicated once more in the castle-engine build tool code
    (toolcompile.pas).
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

{ Although these options are also specified by the CGE build tool
  on the command-line,
  and are in Lazarus packages too,
  we still need to repeat them here.

  That is because user can specify "<option>-Mdelphi</option>"
  in CastleEngineManifest.xml, to compile the application in Delphi mode.
}
{$ifdef FPC}
  {$ifndef CASTLE_ENGINE_TEST_DELPHI_MODE}

    // CASTLE_CONF_DO_NOT_OVERRIDE_MODE is a hack for CastleUtils unit and FPC 3.0.x
    {$ifndef CASTLE_CONF_DO_NOT_OVERRIDE_MODE}
      {$mode objfpc}
      // Using CASTLE_OBJFPC instead of FPC_OBJFPC
      // allows to workaround FPC bug in some units, FPC seems to "lose"
      // the fact that FPC_OBJFPC should be defined.
      // The cleanest solution seems to just ignore FPC_OBJFPC,
      // and define equivalent symbol (CASTLE_OBJFPC) on our own.
      {$define CASTLE_OBJFPC}
    {$endif}

    {$H+}
    {$macro on}
  {$endif}
{$endif}

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

// *Not* define ENABLE_SELF_RECORD_CONSTANTS because:
//
// - Declaring something like TVector3.Zero (constant filled with zeros,
//   inside a record, which type is the same record)
//   causes internal error in FPC 3.0.2, 3.0.4 (not in 3.0.0 or 3.1.1,
//   so already fixed in FPC).
//
// - Declaring something like TVector3.Zero or TVector3.One
//   (any constant inside a record, which type is the same record)
//   is not allowed in Delphi, causes error that type is not completely defined.
//
// - Since FPC 3.3.1 (at least revision 40292, revision 40000 was OK)
//   FPC reports the same error as Delphi
//   (Error: Type "TGenericVector2" is not completely defined).
//
// So ENABLE_SELF_RECORD_CONSTANTS is just *never* defined for now.
// If FPC 3.3.1 will keep this behaviour, we'll just simplify the code
// in the future to not make this optional.
{.$define ENABLE_SELF_RECORD_CONSTANTS}

// Workaround FPC 3.0.0 bug (not present in 3.0.2, 3.0.4, 3.1.1 -- so already fixed).
{$ifdef VER3_0_0}
  {$define BUGGY_CASTLE_VECTORS_DOUBLE_ARRAYS}
{$endif}

{ Nintendo Switch ------------------------------------------------------------ }

{ Since we use (as a hack) Aarch64/Android for Nintendo Switch,
  undefine ANDROID now,
  to not automatically use Android-specific units in
  android/castleandroidinternal*.pas . }
{$ifdef CASTLE_NINTENDO_SWITCH}
  {$undef ANDROID}
{$endif CASTLE_NINTENDO_SWITCH}

{ Platform specific adjustments ---------------------------------------------- }

{ On some Android versions, you cannot call dlopen (load dynamic libraries)
  from the initialization section of units. You have to wait for
  AndroidMain to be called by NDK, otherwise we get a crash at initialization. }
{$define ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
{$ifdef ANDROID}
  {$undef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
{$endif}
// TODO: for now dlopen doesn't work at all on NX, so avoid it
{$ifdef CASTLE_NINTENDO_SWITCH}
  {$undef ALLOW_DLOPEN_FROM_UNIT_INITIALIZATION}
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

{ NX supports OpenGL and OpenGLES (that's public information,
  https://wccftech.com/nintendo-switch-supports-vulkan/ ).
  Our rendering matches better OpenGLES on NX. }
{$ifdef CASTLE_NINTENDO_SWITCH} {$define OpenGLES} {$endif}

{$ifdef OpenGLES}
  {$ifdef CASTLE_ENGINE_TEST_DELPHI_MODE}
    {$error We do not support Android/iOS for Delphi yet, use FPC/Lazarus to develop for Android/iOS and other OpenGLES platforms}
  {$else}
    {$ifdef CASTLE_NINTENDO_SWITCH}
      {$define CastleGL := CastleInternalNxGLES20}
    {$else}
      {$define CastleGL := CastleGLES20}
    {$endif}
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

  1.FpImage (fcl-image units):

    Used if neither CASTLE_PNG_DYNAMIC nor CASTLE_PNG_STATIC are defined
    (you can force it by CASTLE_DISABLE_LIBPNG).
    Or when CASTLE_PNG_DYNAMIC is defined but libpng cannot be loaded.

    Then we use PNG implementation inside fcl-image units family,
    that does not depend on libpng API.
    This is the simplest option to use, since the PNG support is completely
    contained in Pascal code, so you don't need to distribute anything,
    and it works on all platforms.

    The downside is that it is slow.
    Using libpng (see below option) results in drastically faster PNG loading
    (even 4x times).

  2.If CASTLE_PNG_DYNAMIC is defined and libpng exists,
    we use libpng from a dynamic library (.so or .dll).

    - On Linux and other desktop Unixes, libpng is almost definitely already
      installed system-wide, so this just works.

    - On Windows, the appropriate dll is available in our build tool data in
      ../../tools/build-tool/data/external_libraries/ .

      It will be added to your Windows package by the build tool automatically when
      you run "castle-engine package ..."
      See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .
      It will also be placed alongside your .exe when you do "castle-engine compile ...".

    - For macOS, see https://castle-engine.io/macosx_requirements.php .

  3.If CASTLE_PNG_STATIC is defined, we statically link to libpng.
    The presense of static libpng library (libpng.a on Unix) is then
    required at compile-time.
    This is necessary on some platforms, e.g. iOS. The CGE build tool
    automatically uses this flag when necessary.
}

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below

  { On most platforms, try dynamic libpng (will fallback on FpImage at runtime).
    Except on platforms where we know it would not work. }
  {$if not (defined(iOS) or defined(ANDROID) or defined(CASTLE_NINTENDO_SWITCH))}
    {$define CASTLE_PNG_DYNAMIC}
  {$endif}

  {$if defined(CASTLE_PNG_DYNAMIC) and defined(CASTLE_PNG_STATIC)}
    {$error Cannot define both CASTLE_PNG_DYNAMIC and CASTLE_PNG_STATIC. Define at most one of them.}
  {$endif}

  {$if defined(CASTLE_DISABLE_LIBPNG)}
    {$undef CASTLE_PNG_DYNAMIC}
    {$undef CASTLE_PNG_STATIC}
  {$endif}
{$endif}

{ LIBPNG_CDECL is relevant only if we use libpng (dynamic or static).
  Determines calling convention of libpng functions and libpng callbacks.
  When LIBPNG_CDECL is defined, it is "cdecl", otherwise it is "stdcall".
  Right now, in practice it is always LIBPNG_CDECL, even on Windows
  (standard Windows libpng DLL libraries use cdecl, just like on Unix). }
{$ifdef MSWINDOWS}
  {$define LIBPNG_CDECL}
{$else}
  {$define LIBPNG_CDECL}
{$endif}

{ FpHttpClient --------------------------------------------------------------- }

{ Keep HAS_FP_HTTP_CLIENT defined for all platforms that have
  FpHttpClient implemented, if you use FPC. }
{$define HAS_FP_HTTP_CLIENT}
{$ifdef CASTLE_NINTENDO_SWITCH} {$undef HAS_FP_HTTP_CLIENT} {$endif}
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

{ Threading --------------------------------------------------------------- }

{ Threading support, some platform don't have threads (implemented yet). }
{$define CASTLE_SUPPORTS_THREADING}
{$ifdef CASTLE_NINTENDO_SWITCH} {$undef CASTLE_SUPPORTS_THREADING} {$endif}


{$endif not CASTLE_CONF_INCLUDED}
