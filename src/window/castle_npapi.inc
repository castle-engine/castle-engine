{$ifdef UNIX} {$define extdecl := cdecl} {$endif}
{$ifdef MSWINDOWS} {$define extdecl := stdcall} {$endif}

{$ifdef UNIX} {$ifndef DARWIN} {$define MOZ_X11} {$endif} {$endif}
