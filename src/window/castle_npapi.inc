{$ifdef UNIX}
  {$define extdecl := cdecl}
  {$define extdecl_osdecl := cdecl}
{$endif}

{$ifdef MSWINDOWS}
  {$define extdecl := cdecl}
  {$define extdecl_osdecl := stdcall}
{$endif}

{$ifdef UNIX} {$ifndef DARWIN} {$define MOZ_X11} {$endif} {$endif}
