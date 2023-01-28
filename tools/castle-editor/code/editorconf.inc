{$modeswitch advancedrecords}

{ If GENERICS_CONSTREF is defined, then various routines used with Generics.Collections
  (like callbacks we pass to TComparer, or OnNotify callback or Notify virtual method)
  should have "constref" parameter, not "const".
  This was the case of FPC<= 3.2.0, FPC changed it in
  https://gitlab.com/freepascal.org/fpc/source/-/commit/693491048bf2c6f9122a0d8b044ad0e55382354d }
{$ifdef VER3_0} {$define GENERICS_CONSTREF} {$endif}
{$ifdef VER3_2} {$define GENERICS_CONSTREF} {$endif}
