{ include file made for KambiPng.pas unit by Kambi }

{ After including this file use symbol LIBPNG_CDECL / LIBPNG_STDCALL to
  determine calling convention of libpng funcs and callbacks.
}

{$ifdef WIN32}

  { Under win32 define some PNG_Xxx symbol to determine with which dll
    you want to link. }
  {$define PNG_GNUWIN32} { libpng distributed from gnuwin32.sourceforge.net }
  { $define PNG_CYGWIN}  { libpng distributed with Cygwin }
  { $define PNG_VB}      { libpng_vb.dll from http://net.thevbzone.com/d_DLL.htm }

  {$ifdef PNG_GNUWIN32} {$define LIBPNG_CDECL}   {$endif}
  {$ifdef PNG_CYGWIN}   {$define LIBPNG_CDECL}   {$endif}
  {$ifdef PNG_VB}       {$define LIBPNG_STDCALL} {$endif}

{$else}

  { Under linux - just use standard libpng, no problems }
  {$define LIBPNG_CDECL}

{$endif}
