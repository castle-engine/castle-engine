{$ifdef WIN32}
  {$define libvorbisfile_decl := stdcall}
{$else}
  {$define libvorbisfile_decl := cdecl}
{$endif}