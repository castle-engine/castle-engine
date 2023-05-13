{
  Copyright 2002-2023 Michalis Kamburelis.

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

  This file should be included in *all* Castle Game Engine units.
  It defines some symbols that are *not* always configured on FPC command-line
  or in LPK settings, because they cannot, e.g. "modeswitch advancedrecords".

  Note that some required compilation options are still set outside of this file:
  - For example optimization options can only be specified on the command-line
    (and they are important for a game engine).

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

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below
  {$ifdef FPC}
    {$if defined(VER2) or defined(VER3_0)}
      {$fatal You need FPC version >= 3.2.0 to compile Castle Game Engine. See https://castle-engine.io/supported_compilers.php}
    {$endif}
  {$else}
    // See http://delphi.wikia.com/wiki/CompilerVersion_Constant
    {$if CompilerVersion < 20}
      {message fatal 'Use Delphi >= 2009 compile Castle Game Engine. See https://castle-engine.io/supported_compilers.php'}
    {$endif}
  {$endif}
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
  {$mode objfpc}
  {$H+}
  {$macro on}
  {$modeswitch advancedrecords} // Note: no way to specify this by FPC command-line.
{$else}
  { Makes TStructList.L useful in Delphi, like "MyArray.L[100]".
    FPC has this by default.
    See https://docwiki.embarcadero.com/RADStudio/Sydney/en/Pointer_Math_(Delphi) }
  {$pointermath on}
{$endif}

{$writeableconst off} // Note: no way to specify this by FPC command-line.

{ CPU symbols ---------------------------------------------------------------- }

{ Delphi compatibility }
{$ifndef FPC}
  { Define in Delphi CPU32/64, like FPC does. }
  {$ifdef CPUX86} {$define CPU32} {$endif}
  {$ifdef CPU32BITS} {$define CPU32} {$endif}
  {$ifdef CPUX64} {$define CPU64} {$endif}
  {$ifdef CPU64BITS} {$define CPU64} {$endif}

  { Define platforms like FPC does. }
  {$ifdef CPUX64} {$define cpux86_64} {$define cpuamd64} {$endif}
  {$ifdef CPUX86} {$define cpu386} {$define cpui386} {$endif}
  {$ifdef CPUARM64} {$define CPUAARCH64} {$endif}

  { Define in Delphi UNIX when LINUX, like FPC does. }
  {$ifdef LINUX} {$define UNIX} {$endif}
{$endif}

{ Traditionally, CGE defined CPUARM64 also for 64-bit iOS.
  For compatibility, keep it defined there.
  But CPUARM64 should not be used anymore: we use now CPUAARCH64,
  defined by FPC automatically (so it's safer to use, it will work
  regardless of how you compile the sources). }
{$ifdef CPUAARCH64}
  {$define CPUARM64}
{$endif}

{ Endianess ------------------------------------------------------------------ }

{$ifdef FPC}
  {$ifdef FPC_LITTLE_ENDIAN} {$define CASTLE_LITTLE_ENDIAN} {$endif}
  {$ifdef FPC_BIG_ENDIAN} {$define CASTLE_BIG_ENDIAN} {$endif}
{$else}
  { Delphi only supports little-endian it seems. }
  {$define CASTLE_LITTLE_ENDIAN}
{$endif}

{$ifndef CASTLE_LITTLE_ENDIAN}
  {$ifndef CASTLE_BIG_ENDIAN}
    {$error Neither CASTLE_LITTLE_ENDIAN or CASTLE_BIG_ENDIAN defined. Fix castleconf.inc for the current compiler/platform combination.}
  {$endif}
{$endif}

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
{$else}
  { Following https://docwiki.embarcadero.com/RADStudio/Sydney/en/Simple_Types_(Delphi)#Real_Types }
  {$if defined(LINUX) and defined(CPU64)}
    {$define EXTENDED_EQUALS_LONG_DOUBLE}
  {$elseif defined(WIN32)}
    { Original Pascal 10-byte Extended. }
  {$else}
    {$define EXTENDED_EQUALS_DOUBLE}
  {$endif}
{$endif}

{ When using FPC, and Extended = Double, you cannot overload
  for both Double and Extended as they are just equal.
  Extended is just alias for Double.

  Delphi, even when Extended = Double, still treats Extended
  like a different type, for choosing overloads.
  It even requires Extended overloads e.g. for MinVar(Extended, ...). }
{$ifndef PASDOC}
  {$if not (defined(FPC) and defined(EXTENDED_EQUALS_DOUBLE))}
    {$define EXTENDED_OVERLOADS}
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

{ Delphi does not support "inline" with types that are not in units explicitly
  used on the "uses" list.
  Avoid flood of hints like this:
  "[dcc32 Hint] castlerendercontext.pas(313): H2443 Inline function 'TGenericVector4.GetItems' has not been expanded because unit 'CastleVectorsInternalSingle' is not specified in USES list" }
{$if defined(FPC) and defined(SUPPORTS_INLINE)}
  {$define INLINE_GENERIC_VECTORS}
{$endif}

{$define TOBJECT_HAS_EQUALS}

{ Delphi reports many hints/warnings that are known to be harmless in our case. }
{$ifndef FPC}
  {$hints off}
  {$warn COMPARING_SIGNED_UNSIGNED off}
  {$warn COMBINING_SIGNED_UNSIGNED off}
  {$if CompilerVersion >= 32} {$warn COMBINING_SIGNED_UNSIGNED64 off} {$endif} // only since Delphi 10.2
  {$warn IMPLICIT_STRING_CAST off}
  { TODO: We'd like to unhide this. }
  {$warn IMPLICIT_STRING_CAST_LOSS off}
  {$warn GARBAGE off}
  {$warn WIDECHAR_REDUCED off}
  { We sometimes use deprecated CGE things in CGE itself, to support other
    deprecated  API for the time being. We actually disable warnings around with
    $warnings on/off... but not enough for Delphi.
    TODO: This should eventually by unhidden. }
  {$warn SYMBOL_DEPRECATED off}

  { The below are problems only for C++.
    TODO: This should eventually by unhidden and we can support C++ builder. }
  {$warn DUPLICATE_CTOR_DTOR off}
  {$warn PRIVATE_PROPACCESSOR off}
{$endif}

{ If GENERICS_CONSTREF is defined, then various routines used with Generics.Collections
  (like callbacks we pass to TComparer, or OnNotify callback or Notify virtual method)
  should have "constref" parameter, not "const".
  This was the case of FPC<= 3.2.0, FPC changed it in
  https://gitlab.com/freepascal.org/fpc/source/-/commit/693491048bf2c6f9122a0d8b044ad0e55382354d }
{$ifdef VER3_0} {$define GENERICS_CONSTREF} {$endif}
{$ifdef VER3_2} {$define GENERICS_CONSTREF} {$endif}

{ Define CASTLE_IOS when appropriate ----------------------------------------- }

{ Build tool defines CASTLE_IOS automatically.
  This include file defines CASTLE_IOS too, to have it regardless of compilation method.

  Note: FPC >= 3.2.2 introduced own IOS symbol.
  It only indicates "real" physical iOS, not iPhoneSimulator. }

// FPC defines iOS as a separate OS since FPC 3.2.2.
{$define HAS_SEPARATE_IOS}
{$ifdef VER3_0} {$undef HAS_SEPARATE_IOS} {$endif}
{$ifdef VER3_2_0} {$undef HAS_SEPARATE_IOS} {$endif}

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below
  {$ifdef HAS_SEPARATE_IOS}
    {$if defined(iPHONESIM) or defined(iOS)}
      {$define CASTLE_IOS}
    {$endif}
  {$else}
    {$if defined(iPHONESIM) or (defined(DARWIN) and (defined(CPUARM) or defined(CPUAARCH64)))}
      {$define CASTLE_IOS}
    {$endif}
  {$endif}
{$endif}

{ Define COMPILER_CASE_ANALYSIS when appropriate --------------------------- }

{ Compiler automatically detects and warns when "case" doesn't cover
  all possibilities. This means we do not have to, and actually
  we should not have to (as it would cause "Warning: Unreachable code" warning),
  add a clause like "else raise EInternalError.Create('xxx');"
  to "case" statements that should always have a matching possibility.

  This is true for FPC >= 3.2 now. }

{$ifdef FPC}
  {$ifndef VER3_0}
    {$ifndef VER3_1}
      {$define COMPILER_CASE_ANALYSIS}
    {$endif}
  {$endif}
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

(*Whether we use OpenGL ES or normal OpenGL.
  OpenGL ES is by default used for mobile devices.
  For testing, you can also use OpenGL ES on normal desktop OSes
  (see https://castle-engine.io/android_faq#testing-mobile-opengl-es-rendering-without-an-android
  for instructions how to install OpenGL ES on desktop).

  CGE units that directly access OpenGL(ES) should use this line:

    {$ifdef FPC} CastleGL, {$else} OpenGL, OpenGLext, {$endif}

  Applications using CGE (including CGE examples) cannot include castleconf.inc
  (it's internal what it defines) and usually should not need to access OpenGL(ES)
  API directly (to stay independent from OpenGL(ES) details or any future renderer API).
  But if you really have to use OpenGL(ES) API directly, you can do this:

    {$ifdef ANDROID} {$define OpenGLES} {$endif}
    {$ifdef CASTLE_IOS} {$define OpenGLES} {$endif}
    {$ifdef FPC}
      {$ifdef OpenGLES} CastleGLES, {$else} GL, GLExt, {$endif}
    {$else}
      OpenGL, OpenGLext,
    {$endif}

  The above code supports both FPC and Delphi, and supports both OpenGL and OpenGLES.
  You can simplify it in a particular application, if you don't support e.g. both FPC and Delphi.
*)
{ $define OpenGLES}
{$ifdef ANDROID} {$define OpenGLES} {$endif}
{$ifdef CASTLE_IOS} {$define OpenGLES} {$endif}

{ NX supports OpenGL and OpenGLES (that's public information,
  https://wccftech.com/nintendo-switch-supports-vulkan/ ).
  Our rendering matches better OpenGLES on NX. }
{$ifdef CASTLE_NINTENDO_SWITCH} {$define OpenGLES} {$endif}

{$ifdef OpenGLES}
  {$ifndef FPC}
    {$error We do not support OpenGLES (necessary for Android, iOS) with Delphi yet}
  {$else}
    {$ifdef CASTLE_NINTENDO_SWITCH}
      {$define CastleGL := CastleInternalNxGLES20}
    {$else}
      {$define CastleGL := CastleGLES}
    {$endif}
  {$endif}

  { This is necessary for OpenGL ES 2.0 versions that support only 16-bit indexes,
    which is valid for OpenGL ES 2.0 implementations.
    WebGL 1.0 also shares this limitation.

    Example observed in real-life: Android EMUI 3.1
    (Renderer = "Mali-450 MP", Vendor = "ARM", Version string "OpenGL ES 2.0").

    - OpenGLES 2.0 indeed does not allows 32-bit indexes in spec,
      i.e. you cannot pass GL_UNSIGNED_INT to glDrawElements
      ( https://docs.gl/es2/glDrawElements ),
    - although OpenGL 3.0 allows them
      ( https://docs.gl/es3/glDrawElements )
    - and there's an extension to enable it for GLES 2
      ( https://github.com/KhronosGroup/OpenGL-Registry/blob/main/extensions/OES/OES_element_index_uint.txt ).

    Current solution: We render using 16-bit indexes, which may also be useful
    to gain a little performance, *but* it means we cannot render large shapes.

    TODO: The more long-term solution is:

    - Detect the need for 32-bit indexes at runtime,
      and switch between 16-bit and 32-bit indexes as needed.

    - This would allow performance gain on both desktops and mobiles (but I did not
      check whether this gain is noticeable!), while still allowing to render large
      shapes.
      For small shapes, your indexes can take 2x less memory on GPU.

    - Unless we need 32-bit indexes but OpenGLES doesn't support it
      (so we have OpenGLES 2, not 3, and without OES_element_index_uint),
      then we can:

      - refuse to render given shapes (authors that want to support older OpenGLES
        would have to keep this limit in mind, and design for it)
      - or force AllowIndexed := false in X3D renderer
      - or split the shapes (but this would complicate the rendering code).
  }
  {$define GLIndexesShort}
{$else}
  {$define CastleGL := GL, GLExt}
{$endif}

{ PNG ------------------------------------------------------------------------ }

{ There are few ways to read/write the PNG format:

  1.Pascal-only solutions.

    These mean:
    - FpImage (fcl-image units) with FPC
    - PngImage with Delphi
    - Vampyre Imaging Library (both FPC and Delphi)

    These are used:
    - if neither CASTLE_PNG_DYNAMIC nor CASTLE_PNG_STATIC are defined.
    - or CASTLE_DISABLE_LIBPNG is defined (which forcefully undefines
      CASTLE_PNG_DYNAMIC and CASTLE_PNG_STATIC).
    - or when CASTLE_PNG_DYNAMIC is defined but libpng cannot be loaded
      at runtime.

    Pascal solutions are simplest to use, since the PNG support is completely
    contained in Pascal code, so you don't need to distribute anything,
    and it works on all platforms.

    The downside is that it is not optimal.
    In particular FpImage is extremely slow:
    - LibPng results in drastically faster PNG loading than FpImage
    - LibPng gives 4x speedup on desktops over FpImage
    - LibPng gives 10x speedup on Android over FpImage
    Vampyre PNG reading is faster, though LibPng is still ~2x faster than Vampyre.

    Use examples/images_videos/image_display/ to measure the speed.

  2.If CASTLE_PNG_DYNAMIC is defined and libpng exists,
    we use libpng from a dynamic library (.so or .dll).

    - On Linux and other desktop Unixes, libpng is almost definitely already
      installed system-wide, so this just works.

    - On Windows, the appropriate dll is available in our build tool data in
      ../../tools/build-tool/data/external_libraries/ .

      It will be added to your Windows package by the build tool automatically when
      you run "castle-engine package ..."
      See https://castle-engine.io/build_tool .
      It will also be placed alongside your .exe when you do "castle-engine compile ...".

    - On macOS, see https://castle-engine.io/macos .

    - On Android, we have a "png" service with libpng included.
      See ../../tools/build-tool/data/android/integrated-services/png/README.adoc .

  3.If CASTLE_PNG_STATIC is defined, we statically link to libpng.
    The presense of static libpng library (libpng.a on Unix) is then
    required at compile-time.
    This is necessary on some platforms, e.g. iOS. The CGE build tool
    automatically uses this flag when necessary.
}

{$ifndef PASDOC} // Avoid Pasdoc warnings, as Pasdoc cannot calculate $if below

  { On most platforms, try dynamic libpng (will fallback on FpImage at runtime).
    Except on platforms where we know it would not work. }
  {$if not (defined(CASTLE_IOS) or defined(CASTLE_NINTENDO_SWITCH))}
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

{ Threading support, as some platforms don't have threads (implemented yet).

  Note: This does *not* detect the case when threads are possible on this
  platform, but not "compiled in". That is, when using regular Unix
  (like Linux) this symbol will be defined, regardless if you used CThreads or not in FPC. }
{$define CASTLE_SUPPORTS_THREADING}
{$ifdef CASTLE_NINTENDO_SWITCH} {$undef CASTLE_SUPPORTS_THREADING} {$endif}

{ Audio ---------------------------------------------------------------------- }

{ Although Tremolo is largely compatible with VorbisFile, there are small differences,
  e.g. ov_time_total returns Int64 with Tremolo instead of Double with VorbisFile. }
{$if defined(CASTLE_IOS) or defined(ANDROID) or defined(CASTLE_NINTENDO_SWITCH)}
  {$define CASTLE_TREMOLO}
{$endif}

{ X3D ------------------------------------------------------------------------ }

{ Define this if needed to optimize loading time.
  It removes various seldom-used fields from often-used X3D nodes,
  thus increasing the X3D node creation time significantly.
  It also makes some exposed X3D fields into non-exposed
  (it means you cannot define X3D ROUTEs to pass value into/out of this field).
  The choice "which fields are seldom used" is purely subjective, and may change! }
{.$define CASTLE_SLIM_NODES}

{ Vampyre Imaging ------------------------------------------------------------ }

{ Use Vampyre Imaging Library to support additional image formats.
  See https://imaginglib.sourceforge.io/ ,
  https://github.com/galfar/imaginglib ,
  https://castle-engine.io/wp/2021/12/18/integration-with-vampyre-imaging-library-new-example-image_display-to-test-image-loading-speed-and-format-support/
  about Vampyre.

  We rely on Vampyre for a number of formats.

  - With Delphi (where Vampyre is our only way to read some formats,
    like JPG, BPM, PPM).

  - With FPC (although with FPC we can also use FpImage.
    But Vampyre proved better: 2x faster at reading PNG,
    and more reliable at reading BMP, PPM, XPM -- we have testcase images in this
    repo that fail with FpImage, while work OK with Vampyre).

  Right now we just use Vampyre, by default, with all compilers.

  We bundle Vampyre Imaging sources with CGE (in src/vampyre_imaging),
  and so it is available automatically without any effort for all CGE devs,
  and so we just use it automatically.

  Note: To compile with Vampyre using Lazarus (from LPI, LPK):
  remember to register in Lazarus 2 Vampyre packages:
  src/vampyre_imaginglib/src/Packages/VampyreImaginePackage.lpk
  src/vampyre_imaginglib/src/Packages/VampyreImaginePackageExt.lpk
}
{$define USE_VAMPYRE_IMAGING}

{ GPU texture compression ---------------------------------------------------- }

{ If defined, use "astcenc" tool to compress textures to ASTC
  (resulting in ASTC compression in .astc file format).

  Advantage: astcenc is fully open-source, simple tool distributed with CGE.

  Disadvantage: .astc cannot express mipmaps, and we cannot autogenerate mipmaps
  for compressed textures. So this makes such textures bad for situations when
  mipmaps are desired, which is typical for textures in 3D games. }
{.$define USE_ASTCENC}

{ Features ------------------------------------------------------------------- }

(*Generic methods are a cool idea, and we have at least one important use-case
  for them: node searching that filters and returns proper type.
  See TCastleScene.Node, TX3DRootNode.Find.

  Unfortunately generic methods suck now with FPC 3.2.2.
  - In ObjFpc you need to use "specialize" keyword, which looks excessive.
    Esp. that we actually need "{$ifdef FPC}specialize{$endif}".
  - If you forget about "specialize" keyword,
    sometimes you get FPC internal error,
    sometimes it compiles code that always results in EAccessViolation.

  Until this is fixed, we cannot really use them.
  But we have some ready code with them, you can undefine this symbol
  and try out the new generic TCastleScene.Node, TX3DRootNode.Find.
*)
{.$define GENERIC_METHODS}

{ Parts of CastleScript implementation right now use Pascal generics
  in ways that don't compile with Delphi.
  Symbol CASTLE_SCRIPT_FPC is in practice equal to just FPC,
  but having a dedicated symbol allows to easier grep for it and eventually fix. }
{$ifdef FPC}
  {$define CASTLE_SCRIPT_FPC}
{$endif}

{$endif not CASTLE_CONF_INCLUDED}
