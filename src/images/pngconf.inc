{ Include file for PNG library.
  This defines symbol LIBPNG_CDECL or LIBPNG_STDCALL to
  determine calling convention of libpng funcs and callbacks.

  Right now, it's trivial, as all distributed versions (also under Windows)
  use cdecl. }

{$ifdef MSWINDOWS}
  {$define LIBPNG_CDECL}
{$else}
  {$define LIBPNG_CDECL}
{$endif}
